(in-package :jupiter-candle)

(defvar *last-timestamp* 0
  "Helper variable to calculate *elapsed-time*")

(gamekit:defgame jupiter-game ()
  ((gamestate :initform nil :accessor jupiter-gamestate))
  (:viewport-title "Jupiter Candle")
  (:viewport-width 640)
  (:viewport-height 480)
  ;(:fullscreen-p t)
  )

(defun update-elapsed-time ()
  (let ((time (now)))
    (setf *elapsed-time* (* (- time *last-timestamp*) 1000))
    (setf *last-timestamp* time)))

(defmethod gamekit:draw ((this jupiter-game))
  (update-elapsed-time)
  (gamestate-draw (jupiter-gamestate this)))

(defmethod gamekit:post-initialize ((this jupiter-game))
  (setq *universe* (ge.phy:make-universe :2d :on-pre-solve
                                         #'(lambda (this-shape that-shape)
                                             (gamestate-handle-pre-collision (jupiter-gamestate this)
                                                                             this-shape that-shape))))
  (setf (ge.phy:gravity *universe*) (gamekit:vec2 0 -400))
  (setf (jupiter-gamestate this) (make-instance 'gamestate))

  (setf *last-timestamp* (now))

  (gamekit:bind-button :up :pressed #'(lambda () (jump (jupiter-gamestate this))))
  (gamekit:bind-button :left :pressed (lambda () (setf *left-pressed* t)))
  (gamekit:bind-button :left :released (lambda () (setf *left-pressed* nil)))
  (gamekit:bind-button :right :pressed (lambda () (setf *right-pressed* t)))
  (gamekit:bind-button :right :released (lambda () (setf *right-pressed* nil))))

(defmethod gamekit:act ((this jupiter-game))
  (process-timers)
  (loop for i from 0 below *step-split* do
        (ge.phy:observe-universe *universe* (/ *universe-step* *step-split*)))
  (gamestate-step (jupiter-gamestate this)))

(defun main ()
  (gamekit:start 'jupiter-game))
