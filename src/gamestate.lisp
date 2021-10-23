(in-package :jupiter-candle)

(defparameter *universe-step* 0.014)
(defparameter *step-split* 10)

(defparameter *gravity* -800)
(defparameter *jump-force* 35)
(defparameter *dash-force* 27)

(defparameter *max-speed* 150)

(defvar *universe* nil)

(defvar *left-pressed* nil)
(defvar *right-pressed* nil)
(defvar *up-pressed* nil)

(defparameter *first-generator* 'random
  "Specifies the first generator applied at the ground level section.")

(defclass gamestate ()
  ((elements
    :initform (init-level-elements)
    :documentation "List of level elements the player can interact with")
   (tick
    :initform 0
    :documentation "Elapsed game ticks. Incremented on each GAMESTATE-STEP call.")
   (level-height
    :initform 0
    :documentation "Y position of the highest generated level element")
   (player :initform (make-instance 'player :universe *universe*
                                            :position (gamekit:vec2 100 200)))
   (states :initform nil)
   (desired-run-state :initform 0)
   (run-state :initform 0)))

;;-----------------

(defun reinitialize-level (gamestate)
  "Remove all level elements and repopulate the level with (init-level-elements)"
  (with-slots (elements level-height) gamestate
    (dolist (item elements)
      (destroy-element item))
    (setf elements (init-level-elements))
    (setf level-height 0)
    (update-level gamestate 0)))

(defun update-level (gamestate desired-height)
  "Generate level elements at DESIRED-HEIGHT."
  (with-slots (level-height) gamestate
    (let ((generator (if (and (= level-height 0) *first-generator*)
                         *first-generator* (random-generator-name))))
      (loop while (< level-height desired-height) do
            (setf level-height (generate-level-section gamestate level-height
                                                       generator))))))

(defun gamestate-player-animation (gamestate)
  "Return the correct player animation for GAMESTATE"
  (with-slots (states) gamestate
    (cond ((member :dashed-recently states) :dash)
          ((member :run states) :run)
          ((member :on-ground states) :hit-ground)
          ((t :jump)))))

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
          (setf (player-left-oriented-p player) t))
        (if (and *right-pressed* (not *left-pressed*))
            (progn
              (setf desired-run-state 1)
              (setf (player-left-oriented-p player) nil))
            (setf desired-run-state 0)))
    
    (let ((desired-vx (* desired-run-state *max-speed*))
          (vx (gamekit:x (player-speed player)))
          (vy (gamekit:y (player-speed player)))
          (delta-vx (if (player-has-state gamestate :on-ground) 4 0.2)))
      (when (< (abs (- vx desired-vx)) 20)
        (setf delta-vx (/ delta-vx 10)))
      (when (< (abs (- vx desired-vx)) 5)
        (setf delta-vx (/ delta-vx 10)))
      (when (< (abs (- vx desired-vx)) 1)
        (setf delta-vx (/ delta-vx 10)))
      (when (< vx desired-vx)
        (player-apply-impulse player (gamekit:vec2 delta-vx 0)))
      (when (> vx desired-vx)
        (player-apply-impulse player (gamekit:vec2 (- delta-vx) 0)))
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

(defun constrain-player-position (player)
  (let ((position (player-position player))
        (velocity (ge.phy:body-linear-velocity (body player))))
    (when (< (gamekit:x position) 0)
      (setf (player-position player) (gamekit:vec2 0 (gamekit:y position)))
      (when (< (gamekit:x velocity) 0)
        (setf (ge.phy:body-linear-velocity (body player)) (gamekit:vec2 0 (gamekit:y velocity)))))
    (when (>= (gamekit:x position) 640)
      (setf (player-position player) (gamekit:vec2 639 (gamekit:y position)))
      (when (> (gamekit:x velocity) 0)
        (setf (ge.phy:body-linear-velocity (body player)) (gamekit:vec2 0 (gamekit:y velocity)))))))

(defun gamestate-step (gamestate)
  (with-slots (player tick) gamestate
    (incf tick)
    (dolist (item elements)
      (element-act item tick))
    (update-run gamestate)
    (constrain-player-position player)
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
      (player-apply-impulse player (gamekit:vec2 0 *jump-force*)))))

(defun can-dash? (gamestate)
  (and (not (player-has-state gamestate :dashed))
       (not (player-has-state gamestate :dashed-recently))))

(defun dash (gamestate)
  (with-slots (player) gamestate
    (when (can-dash? gamestate)
      (player-change-animation gamestate :dash)
      (player-push-state gamestate :dashed)
      (player-push-state gamestate :dashed-recently)
      (add-timer (+ (now) 0.7)
                 (lambda () (player-remove-state gamestate :dashed-recently)))
      (player-apply-impulse player
                            (gamekit:vec2 (if (player-left-oriented-p player)
                                              (- *dash-force*)
                                              *dash-force*)
                                          (/ *dash-force* 3))
                            :reset-vx t :reset-vy t))))

;;-----------------

(defun gamestate-draw (gamestate)
  (with-slots (player elements states level-height) gamestate
    (let* ((y-speed        (gamekit:y (player-speed player)))
           (staunch-factor (exp (- (/ (abs (max 0 (- y-speed 500))) 7000)))))
      (gamekit:with-pushed-canvas ()
        (ge.vg:scale-canvas 1 staunch-factor)
        (let ((y-offset (ceiling (min 0 (- 150 (gamekit:y (ge.phy:body-position (body player))))))))
          (draw-background y-offset)
          (gamekit:with-pushed-canvas ()
            (gamekit:translate-canvas 0 y-offset)
            (dolist (item elements)
              (render item))
            (render player))))
      (gamekit:draw-text (format nil "Staunch Factor: ~a" staunch-factor)
                         (gamekit:vec2 0 420) :fill-color (gamekit:vec4 1 1 1 1)))
    (gamekit:draw-text (format nil "Player State: ~a" states)
                       (gamekit:vec2 0 460) :fill-color (gamekit:vec4 1 1 1 1))
    (gamekit:draw-text (format nil "Highest Element Y: ~a" level-height)
                       (gamekit:vec2 0 440) :fill-color (gamekit:vec4 1 1 1 1))
    (gamekit:draw-text (format nil "Current height: ~a" (gamekit:y (player-position player)))
                       (gamekit:vec2 0 400) :fill-color (gamekit:vec4 1 1 1 1))))


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
      (player-remove-state gamestate :dashed)
      (player-change-animation gamestate :hit-ground)))
  (setf (ge.phy:collision-friction)         1.0)
  (setf (ge.phy:collision-elasticity)       0.0)
  t)

(defmethod handle-element-pre-collision ((element jump-ring-element) gamestate)
  (when (not (activated element))
    (setf (activated element) t)
    (with-slots (player) gamestate
      (player-apply-impulse player (gamekit:vec2 0 25)))
    (add-timer (+ (now) 2)
               (lambda () (setf (activated element) nil))))
  nil)

