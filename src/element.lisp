(in-package :jupiter-candle)

;;; Elements for collision

(defparameter *level*
  '((:floor :x 0 :y 0 :width 1000)))

;; LEVEL-ELEMENT

(defclass level-element ()
  ((body
    :documentation "The body for the physics simulation")))

(defmethod element-position ((this level-element))
  (with-slots (body) this
    (ge.phy:body-position body)))

;; Floor element

(defclass floor-element (level-element)
  ((width :initarg :width)))

(defmethod initialize-instance :after ((this floor-element) &key position)
  (with-slots (body width) this
    (setf body (ge.phy:make-kinematic-body *universe*))
    (setf (ge.phy:body-position body) position)
    (ge.phy:make-box-shape *universe* width 20 :offset (gamekit:vec2 (/ width 2) 10)
                                               :body body :substance this)))

(defmethod render ((this floor-element))
  (with-slots (width) this
    (gamekit:draw-rect (element-position this) width 20 :fill-paint (gamekit:vec4 1 0 0 1))))

;; Jump Ring

(defclass jump-ring-element (level-element)
  ((activated :initform nil
              :documentation "T if collision effects have already been applied")))

(defmethod initialize-instance :after ((this jump-ring-element) &key position)
  (with-slots (body) this
    (setf body (ge.phy:make-kinematic-body *universe*))
    (setf (ge.phy:body-position body) position)
    (ge.phy:make-box-shape *universe* 100 20 :offset (gamekit:vec2 50 10) :body body :substance this)))

(defmethod render ((this jump-ring-element))
  (with-slots (activated) this
    (gamekit:draw-circle (element-position this) 5 :fill-paint (gamekit:vec4 1 0 0 1))
    (gamekit:draw-rect (element-position this) 100 20 :fill-paint (gamekit:vec4 1 (if activated 1 0.5) 0 1))))

;; Level initialization

(defun init-level-elements ()
  (loop :for item in *level* :collect
        (alexandria:destructuring-ecase item
          ((:jump-ring &key (x 0) (y 0))
           (make-instance 'jump-ring-element
                          :position (gamekit:vec2 x y)))
          ((:floor &key (x 0) (y 0) (width 10))
           (make-instance 'floor-element
                          :position (gamekit:vec2 x y) :width width)))))

