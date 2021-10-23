(in-package :jupiter-candle)

(defparameter *draw-bounding-boxes* nil)

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
  ((width :initarg :width)
   height
   shape))

(defmethod initialize-instance :after ((this boxed-element) &key &allow-other-keys)
  (with-slots (body width height shape) this
    (setf shape (ge.phy:make-box-shape *universe* width height
                                       :body body :substance this))))

(defmethod destroy-element :after ((this boxed-element))
  (with-slots (shape) this
    (ge.ng:dispose shape)))

(defmethod element-origin ((this boxed-element))
  (with-slots (width height) this
    (gamekit:subt (element-position this) (gamekit:vec2 (/ width 2) (/ height 2)))))

(defmethod render :after ((this boxed-element))
  (when *draw-bounding-boxes*
    (with-slots (width height) this
      (gamekit:draw-rect (element-origin this) width height
                         :stroke-paint (gamekit:vec4 1 0 0 1)))))

;; Floor element

(defclass floor-element (boxed-element)
  ((height :initform 10)))

(defmethod render ((this floor-element))
  (with-slots (width height) this
    (gamekit:draw-rect (element-origin this) width height
                       :fill-paint (gamekit:vec4 0.4 0.3 0.8 1))))

;; Jump Ring

(gamekit:define-image jupiter-candle::portal-anim "textures/portal.png")

(defparameter *portal-animations*
  (make-animated-sprite-resource
   'portal-anim 130 100
   '((:active      :row 0 :frames 6 :speed 100)
     (:idle        :row 1 :frames 1))
   :origin (gamekit:vec2 65 50)))

(defclass jump-ring-element (boxed-element)
  ((width :initform 100)
   (height :initform 20)
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
  (with-slots (activated width height) this
    (draw-animated-sprite (slot-value this 'sprite) (element-position this))
    #++(gamekit:draw-rect (element-origin this) width height
                       :fill-paint (gamekit:vec4 1 (if activated 1 0.5) 0 1))))

;; Windmill

;; (defclass windmill-element (level-element)
;;   (shapes :initform nil))

;; (defmethod initialize-instance :after ((this level-element) &key)
;;   (with-slots (shapes body) this
;;     nil))
