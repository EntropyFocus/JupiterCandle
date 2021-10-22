(in-package :jupiter-candle)

(gamekit:define-image jupiter-candle::player-anim "textures/player.png")

(defparameter *player-size* (gamekit:vec2 30 42))
(defparameter *player-origin* (gamekit:div *player-size* 2))

(defparameter *player-animations*
  (make-animated-sprite-resource
   'player-anim 96 84
   '((:idle        :row 1 :frames 7 :speed 100)
     (:idle-to-run :row 2 :frames 2 :speed 100 :next :run)
     (:run         :row 3 :frames 8 :speed 100)
     (:jump        :row 5 :frames 1)
     (:jump-mid    :row 6 :frames 1)
     (:jump-fall   :row 7 :frames 1)
     (:hit-ground  :row 8 :frames 2 :next :idle))
   :origin (gamekit:vec2 (/ 96 2) 21)))


(defclass player ()
  ((body :reader body)
   shape
   (left-oriented :initform nil
                  :accessor player-left-oriented
                  :documentation "player looks to the left side")
   (sprite :initform (make-animated-sprite *player-animations* :idle))))

(defmethod initialize-instance :after ((this player) &key (position (gamekit:vec2 0 0)) universe)
  (with-slots (body shape) this
    (setf body (ge.phy:make-rigid-body universe)
          shape (ge.phy:make-circle-shape universe (/ (gamekit:y *player-size*) 2)
                                          :body body
                                          :substance this)
          (ge.phy:body-position body) position)))

(defun player-position (player)
  (ge.phy:body-position (body player)))

(defun (setf player-position) (position player)
  (setf (ge.phy:body-position (body player)) position))

(defun player-speed (player)
  (ge.phy:body-linear-velocity (body player)))

(defun move-player (player offset)
  (ge.phy:apply-force (body player) (gamekit:mult offset 10000)))

(defmethod render ((this player))
  (let* ((position (player-position this)))
    (gamekit:draw-circle position 5 :fill-paint (gamekit:vec4 1 0 0 1))
    (gamekit:draw-circle position (/ (gamekit:y *player-size*) 2)
                         :stroke-paint (gamekit:vec4 1 0 0 1))
    (draw-animated-sprite (slot-value this 'sprite) position
                          :mirror-x (slot-value this 'left-oriented))))


