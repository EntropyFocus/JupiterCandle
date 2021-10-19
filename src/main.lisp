(in-package :jupiter-candle)

(defparameter *universe-step* 0.014)
(defparameter *step-split* 10)

(defvar *player* nil)
(defvar *universe* nil)

(defvar *on-ground* nil)
(defvar *jumped-recently* 0)
(defvar *left-pressed* nil)
(defvar *right-pressed* nil)
(defvar *up-pressed* nil)

(defvar *desired-run-state* 0) 
(defvar *run-state* 0)

(defun trivial-collide (this-shape that-shape)
  (declare (ignore this-shape))
  (declare (ignore that-shape))
  (setf *on-ground* t))

(defun jump ()
  (if *on-ground*
      (progn
        (setf *on-ground* nil)
        (setf *jumped-recently* 5)
        (move-player *player* (gamekit:vec2 0 15)))
      ()))

(defun update-run ()
  (when *on-ground*
    (if (and *left-pressed* (not *right-pressed*))
        (setf *desired-run-state* -1)
        (if (and *right-pressed* (not *left-pressed*))
            (setf *desired-run-state* 1)
            (setf *desired-run-state* 0)))
    (when (< *run-state* *desired-run-state*)
      (incf *run-state*)
      (move-player *player* (gamekit:vec2 10 0)))
    (when (> *run-state* *desired-run-state*)
      (decf *run-state*)
      (move-player *player* (gamekit:vec2 -10 0)))))

(defun update-on-ground ()
  (when (> *jumped-recently* 0)
    (setf *on-ground* nil)
    (decf *jumped-recently*)))

(gamekit:defgame jupiter-game ()
  ()
  (:viewport-title "Jupiter Candle"))

(defmethod gamekit:draw ((this jupiter-game))
  (render *player*)
  (gamekit:draw-rect (gamekit:vec2 0 10) 1000 2 :fill-paint (gamekit:vec4 1 0 0 1))
  (gamekit:draw-text (format nil "Player pos: ~a"
                             (player-position *player*))
                     (gamekit:vec2 0 0)))

(defmethod gamekit:post-initialize ((this jupiter-game))
  (setq *universe* (ge.phy:make-universe :2d :on-post-solve #'trivial-collide))
  (setf (ge.phy:gravity *universe*) (gamekit:vec2 0 -400))

  (setq *player* (make-player :position (gamekit:vec2 100.0 100.0) :universe *universe*))

  ;; Floor
  (ge.phy:make-box-shape *universe* 1000 2 :offset (gamekit:vec2 0 10))

  (gamekit:bind-button :up :pressed #'jump)
  (gamekit:bind-button :left :pressed (lambda () (setf *left-pressed* t)))
  (gamekit:bind-button :left :released (lambda () (setf *left-pressed* nil)))
  (gamekit:bind-button :right :pressed (lambda () (setf *right-pressed* t)))
  (gamekit:bind-button :right :released (lambda () (setf *right-pressed* nil))))

(defmethod gamekit:act ((this jupiter-game))
  (loop for i from 0 below *step-split* do
        (ge.phy:observe-universe *universe* (/ *universe-step* *step-split*)))
  (update-run)
  (update-on-ground))

(defclass player ()
  ((body :initarg :body
         :reader body)
   (shape :initarg :shape)))

(defun make-player (&key position (universe *universe*))
  (let* ((body (ge.phy:make-rigid-body universe))
         (shape (ge.phy:make-box-shape universe 20 20 :body body)))
    (setf (ge.phy:body-position body) position)
    (make-instance 'player :body body :shape shape)))

(defun player-position (player)
  (ge.phy:body-position (body player)))

(defun set-player-position (player position)
  (setf (ge.phy:body-position (body player)) position))

(defun move-player (player offset)
  (ge.phy:apply-force (body player) (gamekit:mult offset 10000)))

(defmethod render ((this player))
  (let ((position (player-position this)))
    (gamekit:draw-rect position 20 20)
    (gamekit:draw-text "o_O" position)))

(defun main ()
  (gamekit:start 'jupiter-game))
