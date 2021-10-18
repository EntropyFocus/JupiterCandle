(defpackage jupiter-candle
  (:use :cl))
(in-package :jupiter-candle)

(defparameter *universe-step* 0.014)
(defparameter *step-split* 10)

(defvar *player-pos* (gamekit:vec2 200.0 200.0))

(gamekit:defgame jupiter-game ()
  ())

(defmethod gamekit:draw ((this jupiter-game))
  (gamekit:draw-text "o_O" *player-pos*))

(defmethod gamekit:post-initialize ((this jupiter-game))
  ;;(setq *universe* (ge.phy:make-universe :2d))
  ;;(setq *player* (make-instance 'player))
  
  (gamekit:bind-button :up :pressed
                       (lambda ()
                         (setf (gamekit:y *player-pos*) (+ (gamekit:y *player-pos*) 10))))

  (gamekit:bind-button :down :pressed
                       (lambda ()
                         (setf (gamekit:y *player-pos*) (- (gamekit:y *player-pos*) 10))))

  (gamekit:bind-button :left :pressed
                       (lambda ()
                         (setf (gamekit:x *player-pos*) (- (gamekit:x *player-pos*) 10))))

  (gamekit:bind-button :right :pressed
                       (lambda ()
                         (setf (gamekit:x *player-pos*) (+ (gamekit:x *player-pos*) 10)))))


(defclass player ()
  (body
   shape))

;; (defmethod initialize-instance :after ((this player)) &key universe position
;;   (with-slots (body shape) this
;;     (setf body (ge.phy:make-rigid-body universe :mass 10.0)
;;           shape (ge.phy:make-box-shape universe 20 20 :body body)
;;           (ge.phy:body-position body) position)))

;; (defmethod render ((this player))
;;   (with-slots (body) this
;;     (let ((position (ge.phy:body-position body)))
;;       (gamekit:draw-rect position 20 20)
;;       (gamekit:draw-text "o_O" position))))

(defun main ()
  (gamekit:start 'jupiter-game))

