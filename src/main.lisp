(defpackage jupiter-candle
  (:use :cl))
(in-package :jupiter-candle)

(defparameter *universe-step* 0.014)
(defparameter *step-split* 10)

(defvar *player* nil)
(defvar *universe* nil)

(gamekit:defgame jupiter-game ()
  ()
  (:viewport-title "Jupiter Candle"))

(defmethod gamekit:draw ((this jupiter-game))
  (render *player*))

(defmethod gamekit:post-initialize ((this jupiter-game))
  (setq *universe* (ge.phy:make-universe :2d))
  (setq *player* (make-player :position (gamekit:vec2 100.0 100.0) :universe *universe*))

  (flet ((bind-button-to-move (btn xoff yoff)
           (gamekit:bind-button btn :pressed
                                (lambda ()
                                  (move-player *player* (gamekit:vec2 xoff yoff))))))
    (bind-button-to-move :up 0 10)
    (bind-button-to-move :down 0 -10)
    (bind-button-to-move :left -10 0)
    (bind-button-to-move :right 10 0)))


(defclass player ()
  ((body :initarg :body :reader body)
   (shape :initarg :shape)))

(defun make-player (&key position (universe *universe*))
  (let* ((body  (ge.phy:make-rigid-body universe))
         (shape (ge.phy:make-box-shape universe 20 20 :body body)))
    (setf (ge.phy:body-position body) position)
    (make-instance 'player :body body :shape shape)))

(defun player-position (player)
  (ge.phy:body-position (body player)))

(defun set-player-position (player position)
  (setf (ge.phy:body-position (body player)) position))

(defun move-player (player offset)
  (let ((position (player-position player)))
    (set-player-position player (gamekit:add position offset))))

(defmethod render ((this player))
  (let ((position (player-position this)))
    (gamekit:draw-rect position 20 20)
    (gamekit:draw-text "o_O" position)))

(defun main ()
  (gamekit:start 'jupiter-game))

