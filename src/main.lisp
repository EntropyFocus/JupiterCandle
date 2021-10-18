(defpackage jupiter-candle
  (:use :cl))
(in-package :jupiter-candle)

(defvar *player-pos* (gamekit:vec2 200.0 200.0))

(gamekit:defgame jupiter-game ()
  ())

(defmethod gamekit:draw ((this jupiter-game))
  (gamekit:draw-text "o_O" *player-pos*))

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
                       (setf (gamekit:x *player-pos*) (+ (gamekit:x *player-pos*) 10))))

(gamekit:start 'jupiter-game)
