(in-package :jupiter-candle)

;;; Elements for collision


(defparameter *level*
  '((:x 300 :y 130
     :type jump-pad)))

(defclass level-element ()
  ((body
    :documentation "The body for the physics simulation")))

(defmethod initialize-instance ((this level-element) &key universe position)
  (with-slots (body) this
    (setf body (ge.phy:make-kinematic-body universe))
    (setf (ge.phy:body-position body) position)
    (ge.phy:make-box-shape universe 100 20 :offset (gamekit:vec2 50 10) :body body :substance this)))

(defmethod element-position ((this level-element))
  (with-slots (body) this
    (ge.phy:body-position body)))

(defmethod render ((this level-element))
  (gamekit:draw-circle (element-position this) 5 :fill-paint (gamekit:vec4 1 0 0 1))
  (gamekit:draw-rect (element-position this) 100 20 :fill-paint (gamekit:vec4 1 1 0 1)))

(defun init-level-elements ()
  (loop :for item in *level* :collect
    (destructuring-bind (&key (x 0.0) (y 0.0) &allow-other-keys) item
        (make-instance 'level-element
                       :universe *universe*
                       :position (gamekit:vec2 x y)))))

