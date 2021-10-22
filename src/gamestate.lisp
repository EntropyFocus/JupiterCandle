(in-package :jupiter-candle)

(defparameter *universe-step* 0.014)
(defparameter *step-split* 10)

(defparameter *max-speed* 150)

(defvar *universe* nil)

(defvar *left-pressed* nil)
(defvar *right-pressed* nil)
(defvar *up-pressed* nil)

(defvar *level-height* 0)

(defclass gamestate ()
  ((elements
    :initform (init-level-elements)
    :documentation "List of level elements the player can interact with")
   (player :initform (make-instance 'player :universe *universe*
                                            :position (gamekit:vec2 100 200)))
   (states :initform nil)
   (desired-run-state :initform 0)
   (run-state :initform 0)))

;;-----------------

(defun reinitialize-level (gamestate)
  "Remove all level elements and repopulate the level with (init-level-elements)"
  (with-slots (elements) gamestate
    (dolist (item elements)
      (destroy-element item))
    (setf elements (init-level-elements))
    (update-level gamestate *level-height*)))

(defun player-has-state (gamestate state)
  (member state (slot-value gamestate 'states)))

(defun player-push-state (gamestate state)
  "Add STATE to the current set of states. Return T if state has been added."
  (if (player-has-state gamestate state)
      nil
      (progn
        (pushnew state (slot-value gamestate 'states))
        t)))

(defun player-remove-state (gamestate state)
  "Remove STATE from the current set of states. Return T if state has been removed."
  (when (player-has-state gamestate state)
    (with-slots (states) gamestate
      (setf states (delete state states)))
    t))

(defun player-change-animation (gamestate animation)
  (with-slots (player) gamestate
    (with-slots (sprite) player
      (animated-sprite-change-animation sprite animation))))

(defun update-run (gamestate)
  (with-slots (desired-run-state player) gamestate
    (if (and *left-pressed* (not *right-pressed*))
        (progn
          (setf desired-run-state -1)
          (setf (player-left-oriented player) t))
        (if (and *right-pressed* (not *left-pressed*))
            (progn
              (setf desired-run-state 1)
              (setf (player-left-oriented player) nil))
            (setf desired-run-state 0)))
  
    (let ((desired-vx (* desired-run-state *max-speed*))
          (vx (gamekit:x (player-speed player)))
          (vy (gamekit:y (player-speed player)))
          (delta-vx (if (player-has-state gamestate :on-ground) 1 0.2)))
      (when (< (abs (- vx desired-vx)) 20)
        (setf delta-vx (/ delta-vx 10)))
      (when (< (abs (- vx desired-vx)) 5)
        (setf delta-vx (/ delta-vx 10)))
      (when (< (abs (- vx desired-vx)) 1)
        (setf delta-vx (/ delta-vx 10)))
      (when (< vx desired-vx)
        (move-player player (gamekit:vec2 delta-vx 0)))
      (when (> vx desired-vx)
        (move-player player (gamekit:vec2 (- delta-vx) 0)))
      (when (> (abs vy) 10)
        (player-remove-state gamestate :on-ground))
      (if (player-has-state gamestate :on-ground)
        (if (> (abs vx) 5)
          (when (player-push-state gamestate :run)
            (player-change-animation gamestate :idle-to-run))
          (progn
            (when (player-remove-state gamestate :run)
              (player-change-animation gamestate :idle))))
        (progn
          (player-remove-state gamestate :run)
          (when (< vy 0)
            (if (< (abs vx) 40)
                (player-change-animation gamestate :jump-mid)
                (player-change-animation gamestate :jump-fall))))))))

(defun gamestate-step (gamestate)
  (with-slots (player) gamestate
    (update-run gamestate)
    (update-level gamestate (+ 500 (gamekit:y (player-position player))))))

;; ----------------

(defun jump (gamestate)
  (with-slots (player) gamestate
    (when (player-has-state gamestate :on-ground)
      (player-remove-state gamestate :on-ground)
      (when (player-push-state gamestate :jumped-recently)
        (player-change-animation gamestate :jump)
        (add-timer (+ (now) 0.3)
                   (lambda () (player-remove-state gamestate :jumped-recently))))
      (move-player player (gamekit:vec2 0 20)))))

;;-----------------

(defun gamestate-draw (gamestate)
  (with-slots (player elements) gamestate
    (let ((y-offset (ceiling (min 0 (- 150 (gamekit:y (ge.phy:body-position (body player))))))))
      (draw-background y-offset)
      (gamekit:with-pushed-canvas ()
        (gamekit:translate-canvas 0 y-offset)
        (dolist (item elements)
          (render item))
        (render player)))
    (gamekit:draw-text (format nil "Player State: ~a" (slot-value gamestate 'states))
                       (gamekit:vec2 0 460) :fill-color (gamekit:vec4 1 1 1 1))))


;;; COLLISION HANDLING

(defgeneric handle-element-pre-collision (element gamestate)
  (:documentation "Handle the effect of the player colliding with ELEMENT.
Return NIL if collision should be ignored, otherwise T if collision effects
should be applied."))

(defun gamestate-handle-pre-collision (gamestate this-shape that-shape)
  "pre-solve callback to be provided to the physics engine. Return T if
physics engine should apply collision effects."
  (with-slots (player) gamestate
    (let* ((this-sub (ge.phy:shape-substance this-shape))
           (that-sub (ge.phy:shape-substance that-shape)))
      (if (eq this-sub player)
          (handle-element-pre-collision that-sub gamestate)
          (when (eq that-sub player)
            (handle-element-pre-collision this-sub gamestate))))))

(defmethod handle-element-pre-collision ((element floor-element) gamestate)
  (when (not (player-has-state gamestate :jumped-recently))
    (when (player-push-state gamestate :on-ground)
      (player-change-animation gamestate :hit-ground)))
  (setf (ge.phy:collision-friction)         1.0)
  (setf (ge.phy:collision-elasticity)       0.0)
  t)

(defmethod handle-element-pre-collision ((element jump-ring-element) gamestate)
  (with-slots (activated) element
    (when (not activated)
      (setf activated t)
      (with-slots (player) gamestate
        (move-player player (gamekit:vec2 0 25)))
      (add-timer (+ (now) 2)
                 (lambda () (setf activated nil)))))
  nil)

;; Level update, dass elements to the level based on desired height

(defparameter *1-object-1-in* 1.2)
(defparameter *2-object-1-in* 1.2)
(defparameter *platform-1-in* 1.7)
(defparameter *portal-1-in* 1.2)

(defun place-object (gamestate)
  (with-slots (elements) gamestate
    (if (< (random *platform-1-in*) 1)
        (progn
          (push (make-instance 'floor-element :position (gamekit:vec2 (random 900) *level-height*) :width 100) elements))
        (progn
          (when (< (random *portal-1-in*) 1)
            (push (make-instance 'jump-ring-element :position (gamekit:vec2 (random 900) *level-height*)) elements))))
    ))

(defun update-level (gamestate desired-height)
  (with-slots (elements) gamestate
    (loop while (> desired-height *level-height*) do
      (incf *level-height* 50)
      (when (< (random *1-object-1-in*) 1)
        (place-object gamestate)
        (when (< (random *2-object-1-in*) 1)
          (place-object gamestate))))))

