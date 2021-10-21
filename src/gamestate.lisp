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
   (on-ground :initform nil
    :documentation "True if player is connected to the ground.
TODO: replace this with a more generic approach")
   (jumped-recently :initform 0
    :documentation "todo")
   (desired-run-state :initform 0)
   (run-state :initform 0)))

;;-----------------

(defun update-run (gamestate)
  (with-slots (desired-run-state on-ground player) gamestate
    (if (and *left-pressed* (not *right-pressed*))
        (setf desired-run-state -1)
        (if (and *right-pressed* (not *left-pressed*))
            (setf desired-run-state 1)
            (setf desired-run-state 0)))
  
    (let ((desired-vx (* desired-run-state *max-speed*))
          (vx (gamekit:x (player-speed player)))
          (delta-vx (if on-ground 1 0.2)))
      (when (< (abs (- vx desired-vx)) 20)
        (setf delta-vx (/ delta-vx 10)))
      (when (< (abs (- vx desired-vx)) 5)
        (setf delta-vx (/ delta-vx 10)))
      (when (< (abs (- vx desired-vx)) 1)
        (setf delta-vx (/ delta-vx 10)))
      (when (< vx desired-vx)
        (move-player player (gamekit:vec2 delta-vx 0)))
      (when (> vx desired-vx)
        (move-player player (gamekit:vec2 (- delta-vx) 0))))))

(defun update-on-ground (gamestate)
  (with-slots (jumped-recently on-ground) gamestate
    (when (> jumped-recently 0)
      (setf on-ground nil)
      (decf jumped-recently))))

(defun gamestate-step (gamestate)
  (update-run gamestate)
  (update-on-ground gamestate)
  (with-slots (player) gamestate
    (update-level gamestate (+ 20 (gamekit:y (ge.phy:body-position (body player)))))))

;; ----------------

(defun jump (gamestate)
  (with-slots (on-ground player jumped-recently) gamestate
    (when on-ground
      (setf on-ground nil)
      (setf jumped-recently 5)
      (move-player player (gamekit:vec2 0 20)))))

;;-----------------

(defun gamestate-draw (gamestate)
  (with-slots (player elements) gamestate
    (let ((y-offset (- 150 (gamekit:y (ge.phy:body-position (body player))))))
      (draw-background y-offset)
      (gamekit:with-pushed-canvas ()
        (when (< y-offset 0)
          (gamekit:translate-canvas 0 y-offset))
        (dolist (item elements)
          (render item))
        (render player)))))


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
           (that-sub (ge.phy:shape-substance that-shape))
           (element (if (eq this-sub player) that-sub this-sub)))
      (handle-element-pre-collision element gamestate))))

(defmethod handle-element-pre-collision ((element floor-element) gamestate)
  (with-slots (on-ground) gamestate
    (setf on-ground t))
  (setf (ge.phy:collision-friction)         1.0)
  (setf (ge.phy:collision-elasticity)       0.0)
  ;(setf (ge.phy:collision-surface-velocity) 0.0)
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

(defparameter *1-object-1-in* 2)
(defparameter *2-object-1-in* 3)
(defparameter *platform-1-in* 2)
(defparameter *portal-1-in* 2)

(defun place-object (gamestate)
  (with-slots (elements) gamestate
    (if (= 0 (random *platform-1-in*))
        (progn
          (push (make-instance 'floor-element :position (gamekit:vec2 (random 900) *level-height*) :width 100) elements))
        (progn
          (when (= 0 (random *portal-1-in*))
            (push (make-instance 'jump-ring-element :position (gamekit:vec2 (random 900) *level-height*)) elements))))
    )) 

(defun update-level (gamestate desired-height)
  (with-slots (elements) gamestate
    (when (> desired-height *level-height*)
      (incf *level-height* 50)
      (if (= 0 (random *1-object-1-in*))
          (place-object gamestate)
          (progn
            (when (= 0 (random *2-object-1-in*))
              (place-object gamestate)
              #++(place-object gamestate)))))))

