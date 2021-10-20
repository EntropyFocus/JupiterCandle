(in-package :jupiter-candle)


(defclass player ()
  ((body :reader body)
   shape))

(defmethod initialize-instance ((this player) &key (position (gamekit:vec2 0 0)) universe)
  (with-slots (body shape) this
    (setf body (ge.phy:make-rigid-body universe)
          shape (ge.phy:make-box-shape universe
                                       (gamekit:x *player-size*) (gamekit:y *player-size*)
                                       :body body
                                       :substance this
                                       :offset (gamekit:mult *player-size* 0.5))
          (ge.phy:body-position body) position)))

(defun player-position (player)
  (ge.phy:body-position (body player)))

(defun player-speed (player)
  (ge.phy:body-linear-velocity (body player)))

(defun (setf player-position) (position player)
  (setf (ge.phy:body-position (body player)) position))

(defun move-player (player offset)
  (ge.phy:apply-force (body player) (gamekit:mult offset 10000)))

(defmethod render ((this player))
  (let ((position (player-position this)))
    (gamekit:draw-circle position 5 :fill-paint (gamekit:vec4 1 0 0 1))
    (gamekit:draw-rect position (gamekit:x *player-size*) (gamekit:y *player-size*)
                       :stroke-paint (gamekit:vec4 1 0 0 1))
    (gamekit:draw-text "o_O" position)))


