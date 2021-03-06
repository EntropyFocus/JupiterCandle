(in-package :jupiter-candle)

(defparameter *draw-bounding-boxes* nil)

(defvar *element-constructors* (make-hash-table))

(defmacro define-element-constructor (name spec &body body)
  "Define a constructor for a level element. The first argument
of a constructor is LEVEL-HEIGHT denoting the height of the
level section.

Example:

    (define-element-constructor xyz (level-height &key (x 0) (y 0))
      (make-instance 'floor-element 
                     :x x :y y :level-height level-height

A level section generator can then provide an item like

    '(xyz :x 342 :y 842)"
  `(progn
     (setf (gethash ,name *element-constructors*)
           (lambda (,@spec)
             ,@body))))

(defun eval-timed (time val)
  (cond ((functionp val) (funcall val time))
        (t val)))

(defun make-element (level-height spec)
  (let ((constructor (gethash (car spec) *element-constructors*)))
    (apply constructor level-height (cdr spec))))

;; LEVEL-ELEMENT

(defclass level-element ()
  ((body
    :documentation "The body for the physics simulation")
   (level-height :initarg :level-height)
   (x :initarg :x)
   (y :initarg :y)
   (rotation :initform 0 :initarg :rotation)))

(defmethod initialize-instance :after ((this level-element) &key &allow-other-keys)
  (with-slots (body x y rotation level-height) this
    (setf body (ge.phy:make-kinematic-body *universe*))
    (setf (ge.phy:body-position body) (gamekit:vec2 (eval-timed 0 x)
                                                    (+ level-height (eval-timed 0 y))))
    (setf (ge.phy:body-rotation body) (ge.ng:euler-angle->mat2 rotation))))

(defmethod element-position ((this level-element))
  (with-slots (body) this
    (ge.phy:body-position body)))

(defmethod element-speed ((this level-element))
  (with-slots (body) this
    (ge.phy:body-linear-velocity body)))

(defmethod element-rotation ((this level-element))
  (with-slots (body) this
    (ge.ng:mat2->euler-angle (ge.phy:body-rotation body))))

(defmethod render :after ((this level-element))
  (when *draw-bounding-boxes*
    (gamekit:draw-circle (element-position this) 5 :fill-paint (gamekit:vec4 1 0 0 1))))

(defgeneric element-act (element tick))

(defmethod element-act ((this level-element) tick)
  (with-slots (x y body level-height) this
    (when (or (functionp x) (functionp y))
      (let* ((position (ge.phy:body-position body))
             (new-position (gamekit:vec2 (eval-timed tick x)
                                         (+ level-height (eval-timed tick y))))
             (new-velocity (gamekit:subt new-position position)))
        (setf (ge.phy:body-linear-velocity body) new-velocity)))))

(defmethod destroy-element ((this level-element))
  (with-slots (body) this
    (ge.ng:dispose body)))

;; Boxed collision element

(defclass boxed-element (level-element)
  (shape))

(defgeneric element-width (element))
(defgeneric element-height (element))

(defmethod initialize-instance :after ((this boxed-element) &key &allow-other-keys)
  (with-slots (body shape) this
    (setf shape (ge.phy:make-box-shape *universe*
                                       (element-width this) (element-height this)
                                       :body body :substance this))))

(defmethod destroy-element :after ((this boxed-element))
  (with-slots (shape) this
    (ge.ng:dispose shape)))

(defmethod element-origin ((this boxed-element))
  (gamekit:subt (element-position this) (gamekit:vec2 (/ (element-width this) 2)
                                                      (/ (element-height this) 2))))

(defmethod render :after ((this boxed-element))
  (when *draw-bounding-boxes*
    (ge.vg:with-retained-canvas
      (let ((origin (element-position this)))
        (ge.vg:translate-canvas (gamekit:x origin) (gamekit:y origin))
        (ge.vg:rotate-canvas (element-rotation this))
        (ge.vg:translate-canvas (- (/ (element-width this) 2))
                                (- (/ (element-height this) 2)))
        (gamekit:draw-rect (gamekit:vec2 0 0)
                           (element-width this) (element-height this)
                           :stroke-paint (gamekit:vec4 1 0 0 1))))))

;; Ground-like element. Elements of this class behave as elements for the
;; Player to stand on.

(defclass ground-like-element (boxed-element) ())

;; Floor element

(defparameter *floor-element-height* 16)

(defclass floor-element (ground-like-element)
  ((image :initarg :image)))

(defmethod element-width ((this floor-element))
  (with-slots (image) this
    (gamekit:image-width image)))

(defmethod element-height ((this floor-element))
  *floor-element-height*)

(defmethod render ((this floor-element))
  (with-slots (image) this
    (ge.vg:with-retained-canvas
      (let ((origin (element-position this)))
        (ge.vg:translate-canvas (gamekit:x origin) (gamekit:y origin))
        (ge.vg:rotate-canvas (element-rotation this))
        (ge.vg:translate-canvas (- (/ (element-width this) 2))
                                (- (/ (element-height this) 2)))
        (gamekit:draw-image (gamekit:vec2 0 0) image)))))

(dolist (item '(platform-l platform-m platform-s platform-xs ground-floor))
  (define-element-constructor item (level-height &key (x 0) (y 0) (rotation 0))
    (make-instance 'floor-element
                   :level-height level-height
                   :x x :y y :rotation rotation
                   :image item)))

;; Jump Ring

(defparameter *portal-animations*
  (make-animated-sprite-resource
   'portal-anim 130 100
   '((:active      :row 0 :frames 6 :speed 100)
     (:idle        :row 1 :frames 1))
   :origin (gamekit:vec2 65 50)))

(define-element-constructor 'jump-ring (level-height &key (x 0) (y 0))
  (make-instance 'jump-ring-element :level-height level-height :x x :y y))

(defclass jump-ring-element (boxed-element)
  ((width :initform 100 :reader element-width)
   (height :initform 20 :reader element-height)
   (activated :initform nil
              :documentation "T if collision effects have already been applied")
   (sprite :initform (make-animated-sprite *portal-animations* :active))))

(defmethod activated ((this jump-ring-element))
  (slot-value this 'activated))

(defmethod (setf activated) (on (this jump-ring-element))
  (with-slots (sprite activated) this
    (setf activated on)
    (if on
        (animated-sprite-change-animation sprite :idle)
        (animated-sprite-change-animation sprite :active))))

(defmethod render ((this jump-ring-element))
  (with-slots (activated) this
    (draw-animated-sprite (slot-value this 'sprite) (element-position this))))

;; Jump Pad

(defclass jump-pad-element (boxed-element)
  ((width :initform 107 :reader element-width)
   (height :initform 16 :reader element-height)
   (force :initarg :force)))

(defmethod render ((this jump-pad-element))
  (with-slots (width height) this
    (ge.vg:with-retained-canvas
      (let ((origin (element-position this)))
        (ge.vg:translate-canvas (gamekit:x origin) (gamekit:y origin))
        (ge.vg:rotate-canvas (element-rotation this))
        (ge.vg:translate-canvas (- (/ (element-width this) 2))
                                (- (/ (element-height this) 2)))
        (gamekit:draw-image (gamekit:vec2 0 0) 'jump-pad)))))

(define-element-constructor 'jump-pad (level-height &key (x 0) (y 0) (rotation 0) (force 20))
  (make-instance 'jump-pad-element
                 :level-height level-height
                 :x x :y y
                 :rotation rotation
                 :force force))

;; Trampoline Pad

(defclass trampoline-pad-element (boxed-element)
  ((width :initform 107 :reader element-width)
   (height :initform 16 :reader element-height)
   (force :initarg :force)))

(defmethod render ((this trampoline-pad-element))
  (with-slots (width height) this
    (ge.vg:with-retained-canvas
      (let ((origin (element-position this)))
        (ge.vg:translate-canvas (gamekit:x origin) (gamekit:y origin))
        (ge.vg:rotate-canvas (element-rotation this))
        (ge.vg:translate-canvas (- (/ (element-width this) 2))
                                (- (/ (element-height this) 2)))
        (gamekit:draw-image (gamekit:vec2 0 0) 'trampoline-pad)))))

(define-element-constructor 'trampoline-pad (level-height &key (x 0) (y 0) (rotation 0) (force 20))
  (make-instance 'trampoline-pad-element
                 :level-height level-height
                 :x x :y y
                 :rotation rotation
                 :force force))

;; Moving platform

(defparameter *moving-animation*
  (make-animated-sprite-resource
   'moving-platform-anim 107 50
   '((:active      :row 0 :frames 6 :speed 150))
   :origin (gamekit:vec2 53 25)))

(defclass moving-element (ground-like-element)
  ((width :initform 107 :reader element-width)
   (height :initform 16 :reader element-height)
   (sprite :initform (make-animated-sprite *moving-animation* :active))))

(defmethod render ((this moving-element))
  (ge.vg:with-retained-canvas
    (draw-animated-sprite (slot-value this 'sprite) (gamekit:add (element-position this) (gamekit:vec2 0 -13)))))

(define-element-constructor 'hover-pad (level-height &key (x 0) (y 0))
  (make-instance 'moving-element
                 :level-height level-height
                 :x x :y y))

;; Text Element

(defclass text-element (level-element)
  ((text :initarg :text)
   (font :initarg :font)))

(defmethod render ((this text-element))
  (with-slots (text font) this
    (gamekit:draw-text text (gamekit:add (gamekit:vec2 3 -3) (element-position this))
                       :fill-color (gamekit:vec4 0.3 0.1 0.1 1)
                       :font font)
    (gamekit:draw-text text (element-position this)
                       :fill-color (gamekit:vec4 0.7 0.1 0.1 1)
                       :font font)))

(define-element-constructor 'text (level-height &key (text "") (x 0) (y 0) (font-size 32))
  (make-instance 'text-element
                 :level-height level-height
                 :x x :y y
                 :text text
                 :font (gamekit:make-font 'hud-font font-size)))
