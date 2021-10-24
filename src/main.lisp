(in-package :jupiter-candle)

(defvar *last-timestamp* 0
  "Helper variable to calculate *elapsed-time*")

(defparameter *welcome-state* (make-instance 'welcome-state))

(gamekit:defgame jupiter-game ()
  ((gamestate :initform nil :accessor jupiter-gamestate)
   (current-state :initform nil :accessor current-state))
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
  (gamekit:draw-text (format nil "Elapsed time: ~a" *elapsed-time*) (gamekit:vec2 0 0))
  (render (current-state this)))

(defmethod gamekit:post-initialize ((this jupiter-game))
  (setq *universe* (ge.phy:make-universe
                    :2d
                    :on-pre-solve (lambda (this-shape that-shape)
                                    (gamestate-handle-pre-collision (jupiter-gamestate this)
                                                                    this-shape that-shape))
                    :on-post-solve (lambda (this-shape that-shape)
                                     (gamestate-handle-post-collision (jupiter-gamestate this)
                                                                      this-shape that-shape))))
  (setf (ge.phy:gravity *universe*) (gamekit:vec2 0 *gravity*))
  (setf (jupiter-gamestate this) (make-instance 'gamestate))

  (init-sounds)

  (setf *last-timestamp* (now))

  (change-state this *welcome-state*))

(defmethod gamekit:act ((this jupiter-game))
  (process-timers)
  (loop for i from 0 below *step-split* do
        (ge.phy:observe-universe *universe* (/ *universe-step* *step-split*)))
  (act (current-state this)))

(defun change-state (jupiter-game new-state)
  (when (current-state jupiter-game)
    (deactivate (current-state jupiter-game)))
  (setf (current-state jupiter-game) new-state)
  (activate new-state))

(defun start-game (jupiter-game)
  (change-state jupiter-game (slot-value jupiter-game 'gamestate)))

(defun main ()
  (gamekit:start 'jupiter-game))
