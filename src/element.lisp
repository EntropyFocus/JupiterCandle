(in-package :jupiter-candle)

;; LEVEL-ELEMENT

(defclass level-element ()
  ((body
    :documentation "The body for the physics simulation")))

(defmethod element-position ((this level-element))
  (with-slots (body) this
    (ge.phy:body-position body)))

(defmethod render :after ((this level-element))
  (gamekit:draw-circle (element-position this) 5 :fill-paint (gamekit:vec4 1 0 0 1)))

(defmethod destroy-element ((this level-element))
  (with-slots (body) this
    (ge.ng:dispose body)))

;; Boxed collision element

(defclass boxed-element (level-element)
  ((width :initarg :width)
   height
   shape))

(defmethod initialize-instance :after ((this boxed-element) &key position &allow-other-keys)
  (with-slots (body width height shape) this
    (setf body (ge.phy:make-kinematic-body *universe*))
    (setf (ge.phy:body-position body) position)
    (setf shape (ge.phy:make-box-shape *universe* width height
                                       :body body :substance this))))

(defmethod destroy-element :after ((this boxed-element))
  (with-slots (shape) this
    (ge.ng:dispose shape)))

(defmethod element-origin ((this boxed-element))
  (with-slots (width height) this
    (gamekit:subt (element-position this) (gamekit:vec2 (/ width 2) (/ height 2)))))

(defmethod render :after ((this boxed-element))
  (with-slots (width height) this
    (gamekit:draw-rect (element-origin this) width height
                       :stroke-paint (gamekit:vec4 1 0 0 1))))

;; Floor element

(defclass floor-element (boxed-element)
  ((height :initform 10)))

(defmethod render ((this floor-element))
  (with-slots (width height) this
    (gamekit:draw-rect (element-origin this) width height
                       :fill-paint (gamekit:vec4 0.4 0.3 0.8 1))))

;; Jump Ring

(defclass jump-ring-element (boxed-element)
  ((width :initform 100)
   (height :initform 20)
   (activated :initform nil
              :documentation "T if collision effects have already been applied")))

(defmethod render ((this jump-ring-element))
  (with-slots (activated width height) this
    (gamekit:draw-rect (element-origin this) width height
                       :fill-paint (gamekit:vec4 1 (if activated 1 0.5) 0 1))))

