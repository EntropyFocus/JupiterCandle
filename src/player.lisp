(in-package :jupiter-candle)

(gamekit:define-image jupiter-candle::player-anim "textures/player.png")

(defparameter *player-size* (gamekit:vec2 30 80))
(defparameter *player-origin* (gamekit:vec2 15 0))

(defparameter *player-animations*
  (make-animated-sprite-resource
   'player-anim 96 84
   '((:idle        :row 1 :frames 7 :speed 100)
     (:idle-to-run :row 2 :frames 2 :speed 100)
     (:run         :row 3 :frames 8 :speed 100)
     (:jump        :row 5 :frames 1)
     (:jump-mid    :row 6 :frames 1)
     (:jump-fall   :row 7 :frames 1))
   :origin (gamekit:vec2 (/ 96 2) 0)))


(defclass player ()
  ((body :reader body)
   shape
   (sprite :initform (make-animated-sprite *player-animations* :idle))))

(defmethod initialize-instance :after ((this player) &key (position (gamekit:vec2 0 0)) universe)
  (with-slots (body shape) this
    (let ((width (gamekit:x *player-size*))
          (height (gamekit:y *player-size*)))
      (setf body (ge.phy:make-rigid-body universe)
            shape (ge.phy:make-box-shape universe
                                         width
                                         height
                                         :body body
                                         :substance this
                                         :offset (gamekit:vec2 0 (/ height 2)))
            (ge.phy:body-position body) position))))

(defun player-position (player)
  (ge.phy:body-position (body player)))

(defun (setf player-position) (position player)
  (setf (ge.phy:body-position (body player)) position))

(defun player-speed (player)
  (ge.phy:body-linear-velocity (body player)))

(defun move-player (player offset)
  (ge.phy:apply-force (body player) (gamekit:mult offset 10000)))

(defmethod render ((this player))
  (let* ((position (player-position this))
         (origin (gamekit:subt position *player-origin*)))
    (gamekit:draw-circle position 5 :fill-paint (gamekit:vec4 1 0 0 1))
    (gamekit:draw-rect origin (gamekit:x *player-size*) (gamekit:y *player-size*)
                       :stroke-paint (gamekit:vec4 1 0 0 1))
    (draw-animated-sprite (slot-value this 'sprite) position)))


