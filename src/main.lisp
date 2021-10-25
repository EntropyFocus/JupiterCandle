(in-package :jupiter-candle)

(defvar *last-timestamp* 0
  "Helper variable to calculate *elapsed-time*")

(defparameter *welcome-state* (make-instance 'welcome-state))

(gamekit:defgame jupiter-game ()
  ((gamestate :initform nil :accessor jupiter-gamestate)
   (current-state :initform nil :accessor current-state)
   (waiting-state :initform nil :accessor waiting-state))
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
  (gamekit:draw-rect (gamekit:vec2 0 0) 640 480 :fill-paint (gamekit:vec4 0 0 0 1))
  (update-elapsed-time)
  (when (current-state this)
    (render (current-state this))))

(defmethod gamekit:post-initialize ((this jupiter-game))
  (setf *last-timestamp* (now))
  (change-state this *welcome-state*))

(defun all-resources-loaded-p ()
  (= *number-of-loaded-resources* (hash-table-count *requested-resources*)))

(defmethod gamekit:notice-resources ((this jupiter-game) &rest resource-names)
  (loop for name in resource-names
        when (eq (gethash name *requested-resources*) :requested)
        do
        (log:info name)
        (setf (gethash name *requested-resources*) :loaded)
        (incf *number-of-loaded-resources*))
  (let ((next-state (waiting-state this)))
    (when (and next-state (all-resources-loaded-p))
      (setf (waiting-state this) nil)
      (activate next-state)
      (setf (current-state this) next-state))))

(defmethod gamekit:act ((this jupiter-game))
  (process-timers)
  (when (current-state this)
    (act (current-state this))))

(defun change-state (jupiter-game new-state)
  (when (current-state jupiter-game)
    (deactivate (current-state jupiter-game)))
  (setf (current-state jupiter-game) nil)
  (prepare-resources new-state)
  (if (all-resources-loaded-p)
      (progn
        (activate new-state)
        (setf (current-state jupiter-game) new-state))
      (progn
        (setf (waiting-state jupiter-game) new-state))))

(defun start-game (this)
  (setf (jupiter-gamestate this) (make-instance 'gamestate))
  (change-state this (jupiter-gamestate this)))

(defun main ()
  (gamekit:start 'jupiter-game))
