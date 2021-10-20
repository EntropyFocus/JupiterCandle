(in-package :jupiter-candle)

(defparameter *universe-step* 0.014)
(defparameter *step-split* 10)

(defparameter *max-speed* 150)

(defparameter *player-size* (gamekit:vec2 30 80))

(defvar *player* nil)
(defvar *universe* nil)

(defvar *player-v* (gamekit:vec2 0 0))

(defvar *on-ground* nil)
(defvar *jumped-recently* 0)
(defvar *left-pressed* nil)
(defvar *right-pressed* nil)
(defvar *up-pressed* nil)

(defvar *desired-run-state* 0) 
(defvar *run-state* 0)

(defvar *level-elements* nil)
(defvar *ground-shape* nil)

(defvar *activated-level-elements* nil)

(defun pre-collide-handler (this-shape that-shape)
  (let ((other-shape (if (eq this-shape (slot-value *player* 'shape)) that-shape this-shape)))
    (if (eq other-shape *ground-shape*)
        (progn
          (setf *on-ground* t)
          t)
        (pre-collision (ge.phy:shape-substance that-shape)))))

(defgeneric pre-collision (that)
  (:documentation "Pre-collision handling of player and THAT.
Return t if collision should be handled by physics engine."))

(defmethod pre-collision ((that level-element))
  (when (not (find that *activated-level-elements*))
    (push that *activated-level-elements*)
    (move-player *player* (gamekit:vec2 0 50))
    (add-timer (+ (now) 2)
               (lambda () (setf *activated-level-elements* (delete that *activated-level-elements*)))))
  nil)

(defun jump ()
  (if *on-ground*
      (progn
        (setf *on-ground* nil)
        (setf *jumped-recently* 5)
        (move-player *player* (gamekit:vec2 0 15)))
      ()))

(defun update-run ()
  (if (and *left-pressed* (not *right-pressed*))
      (setf *desired-run-state* -1)
      (if (and *right-pressed* (not *left-pressed*))
          (setf *desired-run-state* 1)
          (setf *desired-run-state* 0)))
  
  (let (
        (desired-vx (* *desired-run-state* *max-speed*))
        (vx (gamekit:x *player-v*))
        (delta-vx (if *on-ground* 1 0.2)))
    (when (< (abs (- vx desired-vx)) 20)
      (setf delta-vx (/ delta-vx 10)))
    (when (< (abs (- vx desired-vx)) 5)
      (setf delta-vx (/ delta-vx 10)))
    (when (< (abs (- vx desired-vx)) 1)
      (setf delta-vx (/ delta-vx 10)))
    (when (< vx desired-vx)
      (move-player *player* (gamekit:vec2 delta-vx 0)))
    (when (> vx desired-vx)
      (move-player *player* (gamekit:vec2 (- delta-vx) 0)))
    )
  )

(defun update-on-ground ()
  (when (> *jumped-recently* 0)
    (setf *on-ground* nil)
    (decf *jumped-recently*)))

(gamekit:defgame jupiter-game ()
  ()
  (:viewport-title "Jupiter Candle"))

(defmethod gamekit:draw ((this jupiter-game))
  (render *player*)
  (dolist (item *level-elements*)
    (render item))
  (gamekit:draw-rect (gamekit:vec2 0 10) 1000 2 :fill-paint (gamekit:vec4 1 0 0 1))
  (gamekit:draw-text (format nil "Player pos: ~a"
                             (player-position *player*))
                     (gamekit:vec2 0 0)))

(defmethod gamekit:post-initialize ((this jupiter-game))
  (setq *universe* (ge.phy:make-universe :2d :on-pre-solve #'pre-collide-handler))
  (setf (ge.phy:gravity *universe*) (gamekit:vec2 0 -400))

  (setq *player* (make-player :position (gamekit:vec2 100.0 100.0) :universe *universe*))

  ;; Floor
  (setq *ground-shape* (ge.phy:make-box-shape *universe* 1000 2 :offset (gamekit:vec2 0 10)))

  (setq *level-elements* (init-level-elements))

  (gamekit:bind-button :up :pressed #'jump)
  (gamekit:bind-button :left :pressed (lambda () (setf *left-pressed* t)))
  (gamekit:bind-button :left :released (lambda () (setf *left-pressed* nil)))
  (gamekit:bind-button :right :pressed (lambda () (setf *right-pressed* t)))
  (gamekit:bind-button :right :released (lambda () (setf *right-pressed* nil))))

(defmethod gamekit:act ((this jupiter-game))
  (process-timers)
  (loop for i from 0 below *step-split* do
        (ge.phy:observe-universe *universe* (/ *universe-step* *step-split*)))
  (update-run)
  (update-on-ground)
  (setf *player-v* (ge.phy:body-linear-velocity (body *player*))))

(defclass player ()
  ((body :initarg :body
         :reader body)
   (shape :initarg :shape)))

(defun make-player (&key position (universe *universe*))
  (let* ((body (ge.phy:make-rigid-body universe))
         (shape (ge.phy:make-box-shape universe
                                       (gamekit:x *player-size*) (gamekit:y *player-size*)
                                       :body body
                                       :offset (gamekit:mult *player-size* 0.5))))
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
    (gamekit:draw-circle position 5 :fill-paint (gamekit:vec4 1 0 0 1))
    (gamekit:draw-rect position (gamekit:x *player-size*) (gamekit:y *player-size*)
                       :stroke-paint (gamekit:vec4 1 0 0 1))
    (gamekit:draw-text "o_O" position)))

(defun main ()
  (gamekit:start 'jupiter-game))
