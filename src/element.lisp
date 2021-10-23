(in-package :jupiter-candle)

(defparameter *draw-bounding-boxes* nil)

(defvar *element-constructors* (make-hash-table))

(defmacro define-element-constructor (name spec &body body)
  `(progn
     (setf (gethash ,name *element-constructors*)
           (lambda (,@spec)
             ,@body))))

(defun make-element (level-height spec)
  (let ((constructor (gethash (car spec) *element-constructors*))
        (x (getf (cdr spec) :x))
        (y (getf (cdr spec) :y)))
    (funcall constructor (gamekit:vec2 x (+ level-height y)))))

;; LEVEL-ELEMENT

(defclass level-element ()
  ((body
    :documentation "The body for the physics simulation")))

(defmethod initialize-instance :after ((this level-element) &key position &allow-other-keys)
  (with-slots (body) this
    (setf body (ge.phy:make-kinematic-body *universe*))
    (setf (ge.phy:body-position body) position)))

(defmethod element-position ((this level-element))
  (with-slots (body) this
    (ge.phy:body-position body)))

(defmethod render :after ((this level-element))
  (when *draw-bounding-boxes*
    (gamekit:draw-circle (element-position this) 5 :fill-paint (gamekit:vec4 1 0 0 1))))

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
    (gamekit:draw-rect (element-origin this)
                       (element-width this) (element-height this)
                       :stroke-paint (gamekit:vec4 1 0 0 1))))

;; Floor element

(gamekit:define-image jupiter-candle::ground-floor "textures/ground_floor.png")
(gamekit:define-image jupiter-candle::platform-l "textures/platform_l.png")
(gamekit:define-image jupiter-candle::platform-m "textures/platform_m.png")
(gamekit:define-image jupiter-candle::platform-s "textures/platform_s.png")
(gamekit:define-image jupiter-candle::platform-xs "textures/platform_xs.png")
(defparameter *floor-element-height* 16)

(defclass floor-element (boxed-element)
  ((image :initarg :image)))

(defmethod element-width ((this floor-element))
  (with-slots (image) this
    (gamekit:image-width image)))

(defmethod element-height ((this floor-element))
  *floor-element-height*)

(defmethod render ((this floor-element))
  (with-slots (image) this
    (gamekit:draw-image (element-origin this) image)))

(dolist (item '(platform-l platform-m platform-s platform-xs ground-floor))
  (define-element-constructor item (position)
    (make-instance 'floor-element :position position :image item)))

;; Jump Ring

(gamekit:define-image jupiter-candle::portal-anim "textures/portal.png")

(defparameter *portal-animations*
  (make-animated-sprite-resource
   'portal-anim 130 100
   '((:active      :row 0 :frames 6 :speed 100)
     (:idle        :row 1 :frames 1))
   :origin (gamekit:vec2 65 50)))

(define-element-constructor 'jump-ring (position)
  (make-instance 'jump-ring-element :position position))

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

;; Windmill

;; (defclass windmill-element (level-element)
;;   (shapes :initform nil))

;; (defmethod initialize-instance :after ((this level-element) &key)
;;   (with-slots (shapes body) this
;;     nil))
