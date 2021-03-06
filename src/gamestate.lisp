(in-package :jupiter-candle)

(defparameter *universe-step* 0.014)
(defparameter *step-split* 10)

(defparameter *gravity* -800)
(defparameter *jump-force* 35)
(defparameter *dash-force* 27)

(defparameter *max-speed* 150)
(defparameter *bump-threshold* (expt (+ 50 *max-speed*) 2))


(defvar *universe* nil)

(defvar *left-pressed* nil)
(defvar *right-pressed* nil)
(defvar *up-pressed* nil)

(defparameter *first-generator* 'tutorial
  "Specifies the first generator applied at the ground level section.")

(defclass gamestate ()
  ((elements
    :initform nil
    :documentation "List of level elements the player can interact with")
   (tick
    :initform 0
    :documentation "Elapsed game ticks. Incremented on each GAMESTATE-STEP call.")
   (height-record
    :initform 0
    :documentation "Maximum height achieved by the player so far.")
   (level-height
    :initform 0
    :documentation "Y position of the highest generated level element")
   (player :initform nil)
   (current-player-anim
    :initform nil
    :documentation "Currently set player animation state.")
   (ground-element
    :initform nil
    :documentation "Last ground element that the player was standing on.")
   (last-ground-contact
    :initform 0
    :documentation "Last tick of collision with ground.")
   (desired-run-velocity
    :initform (gamekit:vec2 0 0)
    :documentation "Desired player velocity while standing on a platform.")
   (states
    :initform nil
    :documentation "Set of active gamestates as assoc-list (STATE . TICK-START)
where TICK-START denotes the tick-time when the state was activated.")))

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
    (loop while (< level-height desired-height) do
          (let ((generator (if (and (= level-height 0) *first-generator*)
                               *first-generator* (random-generator-name :excluding '(tutorial)))))
            (setf level-height (generate-level-section gamestate level-height
                                                       generator))))))

(defun player-change-animation (gamestate animation)
  (with-slots (player current-player-anim) gamestate
    (when (not (eq current-player-anim animation))
      (setf current-player-anim animation)
      (with-slots (sprite) player
        (animated-sprite-change-animation sprite animation)))))

(defun reset-player-animation (gamestate)
  "Return the correct player animation for GAMESTATE"
  (with-slots (states) gamestate
    (let ((anim (cond ((assoc :dashing states) :dash)
                      ((assoc :run-jump states) :run-jump)
                      ((assoc :running states) :run)
                      ((assoc :on-ground states) :hit-ground)
                      ((assoc :jump-mid states) :jump-mid)
                      ((assoc :has-jumped states) :jump)
                      ((assoc :falling states) :jump)
                      (t :idle))))
      (player-change-animation gamestate anim))))

(defun on-state-added (gamestate added-state)
  (declare (ignore gamestate))
  (case added-state
    (:has-jumped (gamekit:play-sound 'jump-sound))
    (:has-dashed (gamekit:play-sound 'dash-sound))
    (:bumped (gamekit:play-sound 'bump-sound))
    (:weee  (gamekit:play-sound 'weee-sound))
    (:big-fall (gamekit:play-sound 'aaah-sound))
    (:hard-ground-hit (gamekit:play-sound 'hit-ground-sound))))

(defun on-state-removed (gamestate removed-state)
  (declare (ignore gamestate))
  (case removed-state
    (:big-fall (gamekit:stop-sound 'aaah-sound))))

(defun player-local-speed (gamestate)
  "When standing on a platform that moves the player, the local
player speed denotes the speed relative to that platform. In every
other case, the speed is the same as the global player speed."
  (with-slots (ground-element player) gamestate
    (let ((speed (player-speed player)))
      (if (and ground-element (player-has-state gamestate :on-ground))
          (let ((e-speed (element-speed ground-element)))
            (gamekit:subt speed e-speed))
          speed))))

(defun compute-state-changes (gamestate)
  (let* ((speed (player-local-speed gamestate))
         (vx (gamekit:x speed))
         (vy (gamekit:y speed)))
    (with-slots (tick last-ground-contact) gamestate
      (when (or (> (- tick last-ground-contact) 5) (> (abs vy) 10))
        (player-remove-state gamestate :on-ground)))

    (player-remove-state gamestate :dashing :if-older-than 26)
    (player-remove-state gamestate :bumped :if-older-than 30)

    ;; Compute before :running
    (when (and (player-has-state gamestate :running)
               (player-has-state gamestate :has-jumped))
      (player-push-state gamestate :run-jump))

    (when (player-has-state gamestate :on-ground)
      (player-remove-state gamestate :has-jumped)
      (player-remove-state gamestate :run-jump)
      (player-remove-state gamestate :has-dashed))

    (player-set-state gamestate :running 
                      (and (player-has-state gamestate :on-ground)
                           (or *left-pressed* *right-pressed*)
                           (> (abs vx) 5)))

    (player-set-state gamestate :idleing
                      (and (player-has-state gamestate :on-ground)
                           (not (player-has-state gamestate :running))
                           (not (player-has-state gamestate :dashing))))

    (player-set-state gamestate :jump-mid
                      (and (player-has-state gamestate :has-jumped)
                           (< (abs vx) 150)))

    (player-set-state gamestate :falling
                      (and (not (player-has-state gamestate :on-ground))
                           (< vy -100)))

    (player-set-state gamestate :fast-ascend (> vy 1500))
    (player-set-state gamestate :weee
                      (player-has-state gamestate :fast-ascend :at-least-ticks 15))

    ;; Compute before :big-fall
    (player-set-state gamestate :hard-ground-hit
                      (and (player-has-state gamestate :big-fall)
                           (player-has-state gamestate :on-ground)))

    (player-set-state gamestate :big-fall
                      (and (< vy -1000)
                           (player-has-state gamestate :falling :at-least-ticks 80)))))

(defun player-has-state (gamestate state &key (at-least-ticks 0))
  (with-slots (tick) gamestate
    (alexandria:when-let ((start-tick (alexandria:assoc-value (slot-value gamestate 'states) state)))
      (>= (- tick start-tick) at-least-ticks))))

(defun player-push-state (gamestate state)
  "Add STATE to the current set of states. Return T if state has been added."
  (if (player-has-state gamestate state)
      nil
      (with-slots (states tick) gamestate
        (push (cons state tick) states)
        (on-state-added gamestate state)
        t)))

(defun player-remove-state (gamestate state &key (if-older-than 0))
  "Remove STATE from the current set of states. Return T if state has been removed."
  (when (player-has-state gamestate state :at-least-ticks if-older-than)
    (with-slots (states) gamestate
      (alexandria:removef states state :key #'car)
      (on-state-removed gamestate state))
    t))

(defun player-set-state (gamestate state &optional (on t))
  (if on
      (player-push-state gamestate state)
      (player-remove-state gamestate state)))

(defun update-run (gamestate)
  (with-slots (player desired-run-velocity) gamestate
    (let ((desired-vx
            (cond
              ((and *left-pressed* (not *right-pressed*))
               (setf (player-left-oriented-p player) t)
               (- *max-speed*))
              ((and *right-pressed* (not *left-pressed*))
               (setf (player-left-oriented-p player) nil)
               *max-speed*)
              (t 0)))
          (vx (gamekit:x (player-local-speed gamestate)))
          (delta-vx (if (player-has-state gamestate :on-ground) 4 0.2)))
      (when (< (abs (- vx desired-vx)) 20)
        (setf delta-vx (/ delta-vx 10)))
      (when (< (abs (- vx desired-vx)) 5)
        (setf delta-vx (/ delta-vx 10)))
      (when (< (abs (- vx desired-vx)) 1)
        (setf delta-vx (/ delta-vx 10)))
      (setf desired-run-velocity (gamekit:vec2 desired-vx 0))
      (when (not (player-has-state gamestate :on-ground))
        (cond
          ((< vx desired-vx) (player-apply-impulse player (gamekit:vec2 delta-vx 0)))
          ((> vx desired-vx) (player-apply-impulse player (gamekit:vec2 (- delta-vx) 0))))))))

(defun update-player-on-ground (gamestate)
  "Update the player speed relative to the platform the player is standing on."
  (with-slots (ground-element player desired-run-velocity) gamestate
    (when (and (player-has-state gamestate :on-ground :at-least-ticks 2) ground-element)
      (let ((e-speed (element-speed ground-element)))
        (setf (player-speed player) (gamekit:add e-speed desired-run-velocity))))))

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

(defmethod act ((this gamestate))
  (loop for i from 0 below *step-split* do
        (ge.phy:observe-universe *universe* (/ *universe-step* *step-split*))
        (update-player-on-ground this))
  (with-slots (player tick elements height-record) this
    (incf tick)
    (setf height-record (max height-record (gamekit:y (player-position player))))
    (dolist (item elements)
      (element-act item tick))
    (compute-state-changes this)
    (reset-player-animation this)
    (update-run this)
    (constrain-player-position player)
    (update-level this (+ 500 (gamekit:y (player-position player))))))

;; ----------------

(defun jump (gamestate)
  (with-slots (player) gamestate
    (when (player-has-state gamestate :on-ground)
      (player-remove-state gamestate :on-ground)
      (player-push-state gamestate :has-jumped)
      (player-apply-impulse player (gamekit:vec2 0 *jump-force*)))))

(defun can-dash? (gamestate)
  (not (player-has-state gamestate :has-dashed)))

(defun dash (gamestate)
  (with-slots (player) gamestate
    (when (can-dash? gamestate)
      (player-push-state gamestate :has-dashed)
      (player-push-state gamestate :dashing)
      (player-remove-state gamestate :on-ground)
      (player-apply-impulse player
                            (gamekit:vec2 (if (player-left-oriented-p player)
                                              (- *dash-force*)
                                              *dash-force*)
                                          (/ *dash-force* 3))
                            :reset-vx t :reset-vy t))))

;;-----------------

(defmethod prepare-resources ((this gamestate))
  (load-game-resources))

(defmethod activate ((this gamestate))
  (setq *universe* (ge.phy:make-universe
                    :2d
                    :on-pre-solve (lambda (this-shape that-shape)
                                    (gamestate-handle-pre-collision this
                                                                    this-shape that-shape))
                    :on-post-solve (lambda (this-shape that-shape)
                                     (gamestate-handle-post-collision this
                                                                      this-shape that-shape))))
  (setf (ge.phy:gravity *universe*) (gamekit:vec2 0 *gravity*))

  (with-slots (player) this
    (setf player (make-instance 'player :universe *universe*
                                        :position (gamekit:vec2 100 200))))
  (reinitialize-level this)

  (setf (gamekit:sound-gain 'dash-sound) 2.0)
  (setf (gamekit:sound-gain 'jump-sound) 2.0)
  (setf (gamekit:sound-gain 'bump-sound) 2.0)
  (setf (gamekit:sound-gain 'weee-sound) 0.7)
  (setf (gamekit:sound-gain 'aaah-sound) 1.5)
  (setf (gamekit:sound-gain 'hit-ground-sound) 2.0)
  (setf (gamekit:sound-gain 'portal-sound) 2.0)
  (setf (gamekit:sound-gain 'game-sound) 0.2)
  
  (gamekit:bind-button :up :pressed (lambda () (jump this)))
  (gamekit:bind-button :left :pressed (lambda () (setf *left-pressed* t)))
  (gamekit:bind-button :left :released (lambda () (setf *left-pressed* nil)))
  (gamekit:bind-button :right :pressed (lambda () (setf *right-pressed* t)))
  (gamekit:bind-button :right :released (lambda () (setf *right-pressed* nil)))

  (gamekit:bind-button :C :pressed (lambda () (dash this)))

  (gamekit:bind-button :R :pressed
                       (lambda ()
                         (reinitialize-level this)))
  
  (gamekit:play-sound 'game-sound :looped-p t))

(defmethod deactivate ((this gamestate))
  (gamekit:bind-button :up :pressed nil)
  (gamekit:bind-button :left :pressed nil)
  (gamekit:bind-button :left :released nil)
  (gamekit:bind-button :right :pressed nil)
  (gamekit:bind-button :right :released nil)

  (gamekit:bind-button :C :pressed nil)

  (gamekit:bind-button :R :pressed nil)
  
  (gamekit:stop-sound 'game-sound))

(defun draw-height-record (height-record)
  (gamekit:draw-line (gamekit:vec2 0 height-record)
                     (gamekit:vec2 640 height-record)
                     (gamekit:vec4 0.4 0.1 0.1 1)
                     :thickness 4.0))

(defmethod render ((this gamestate))
  (with-slots (player elements states level-height height-record desired-run-velocity) this
    (let* ((y-speed        (gamekit:y (player-speed player)))
           (staunch-factor (exp (- (/ (abs (max 0 (- y-speed 500))) 7000)))))
      (gamekit:with-pushed-canvas ()
        (ge.vg:scale-canvas 1 staunch-factor)
        (let ((y-offset (ceiling (min 0 (- 150 (gamekit:y (ge.phy:body-position (body player))))))))
          (draw-background y-offset)
          (gamekit:with-pushed-canvas ()
            (gamekit:translate-canvas 0 y-offset)
            (draw-height-record height-record)
            (dolist (item elements)
              (render item))
            (render player)))))
    #++(gamekit:draw-text (format nil "State: ~a" states)
                       (gamekit:vec2 1 -0.9) :fill-color (gamekit:vec4 0 0 0 1))
    #++(gamekit:draw-text (format nil "State: ~a" states)
                       (gamekit:vec2 0 0) :fill-color (gamekit:vec4 1 1 1 1))
    (let ((font (gamekit:make-font 'hud-font 24))
          (text (format nil "Height Record: ~6,'0D" (floor height-record))))
      (multiple-value-bind (origin width height) (gamekit:calc-text-bounds text font)
        (declare (ignore origin))
        (gamekit:draw-text text
                           (gamekit:subt (gamekit:vec2 640 480) (gamekit:vec2 width height))
                           :fill-color (gamekit:vec4 1 1 1 1)
                           :font font)))))


;;; COLLISION HANDLING

(defgeneric handle-element-pre-collision (element gamestate)
  (:documentation "Handle the effect of the player colliding with ELEMENT.
Return NIL if collision should be ignored, otherwise T if collision effects
should be applied."))

(defgeneric handle-element-post-collision (element gamestate)
  (:documentation "Handle the after-effect of the player colliding 
with ELEMENT.")
  (:method (element gamestate)
    (declare (ignore element))
    (declare (ignore gamestate))
    nil))


(defun gamestate-handle-pre-collision (gamestate this-shape that-shape)
  "pre-solve callback to be provided to the physics engine. Return T if
physics engine should apply collision effects."
  (with-slots (player) gamestate
    (let* ((this-sub (ge.phy:shape-substance this-shape))
           (that-sub (ge.phy:shape-substance that-shape)))
      (cond
        ((eq this-sub player) (handle-element-pre-collision that-sub gamestate))
        ((eq that-sub player) (handle-element-pre-collision this-sub gamestate))))))

(defun gamestate-handle-post-collision (gamestate this-shape that-shape)
  (with-slots (player) gamestate
    (let* ((this-sub (ge.phy:shape-substance this-shape))
           (that-sub (ge.phy:shape-substance that-shape)))
      (cond
        ((eq this-sub player) (handle-element-post-collision that-sub gamestate))
        ((eq that-sub player) (handle-element-post-collision this-sub gamestate))))))

(defmethod handle-element-pre-collision ((element ground-like-element) gamestate)
  (with-slots (player ground-element last-ground-contact tick) gamestate
    (let ((p-pos (player-position player))
          (e-pos (element-position element)))
      ;; This could be problematic when element is rotated. Need to check which side was bumped.
      ;; Maybe easier to tell via another shape??
      (when (> (gamekit:y p-pos) (gamekit:y e-pos))
        (player-push-state gamestate :on-ground)
        (setf ground-element element)
        (setf last-ground-contact tick)))
    (let* ((player-speed (player-speed player))
           (element-speed (element-speed element))
           (diff (gamekit:subt player-speed element-speed)))
      (when (> (+ (expt (gamekit:x diff) 2) (expt (gamekit:y diff) 2)) *bump-threshold*)
        (player-push-state gamestate :bumped)))
    (setf (ge.phy:collision-friction)         0.0)
    (setf (ge.phy:collision-elasticity)       0.0)
    t))

(defmethod handle-element-pre-collision ((element jump-ring-element) gamestate)
  (when (not (activated element))
    (setf (activated element) t)
    (gamekit:play-sound 'portal-sound)
    (with-slots (player) gamestate
      (player-apply-impulse player (gamekit:vec2 0 25)))
    (add-timer (+ (now) 2)
               (lambda () (setf (activated element) nil))))
  nil)

(defmethod handle-element-pre-collision ((element jump-pad-element) gamestate)
  (with-slots (player) gamestate
    (when (> (gamekit:y (player-position player)) (gamekit:y (element-position element)))
      (setf (ge.phy:collision-elasticity) 1.0)
      (with-slots (force) element
        (player-apply-impulse player (gamekit:vec2 0 force))))))
