(in-package :jupiter-candle)

(defvar *elapsed-time* 0
  "Elapsed time in ms since the last draw operation")

(defclass animated-sprite-resource ()
  ((image
    :initarg :image
    :documentation "name of image resource")
   (width
    :initarg :width
    :documentation "width of an animated frame")
   (height
    :initarg :height
    :documentation "height of an animated frame")
   (origin
    :initarg :origin
    :documentation "origin point")
   (animations
    :initform (make-hash-table))))

(defun make-animated-sprite-resource (image width height animations-spec
                                      &key (origin (gamekit:vec2 0 0)))
  "Create a new ANIMATED-SPRITE-RESOURCE based on the tileset image IMAGE
where each tile has the size (WIDTH, HEIGHT). ANIMATIONS-SPEC is list of
named animations. ORIGIN marks the origin point of the sprite.

Each list item has the structure

   (NAME &key ROW FRAMES SPEED)."
  (let ((this (make-instance 'animated-sprite-resource
                             :image image
                             :width width
                             :height height
                             :origin origin)))
    (with-slots (animations) this
      (dolist (item animations-spec)
        (setf (gethash (car item) animations) (cdr item))))
    this))

(defun animation-spec (resource name)
  (gethash name (slot-value resource 'animations)))

(defun draw-animated-sprite-resource (resource position animation frame &key mirror-x)
  (with-slots (image width height origin) resource
    (let* ((row  (getf (animation-spec resource animation) :row))
           (x    (* frame width))
           (y    (* row height)))
      (gamekit:draw-image (gamekit:subt position origin)
                          image
                          :origin (gamekit:vec2 x y)
                          :width width :height height :mirror-x mirror-x))))

(defclass animation-state ()
  ((timestamp :initform 0)
   frametime
   total-length
   frame
   image
   y
   width
   height
   origin))

(defun update-animation-state (state)
  (with-slots (timestamp total-length frametime frame) state
    (setf timestamp (mod (+ timestamp *elapsed-time*) total-length)
          frame     (floor timestamp frametime))))

(defun make-animation-state (resource animation-name)
  (destructuring-bind
      (&key (row 0) (frames 1) (speed 100))
      (animation-spec resource animation-name)
    (let* ((state (make-instance 'animation-state)))
      (with-slots (total-length image y width height frametime origin) state
        (setf total-length (* frames speed)
              image (slot-value resource 'image)
              height (slot-value resource 'height)
              y (* (1+ row) height)
              width (slot-value resource 'width)
              frametime speed
              origin (slot-value resource 'origin)))
      (update-animation-state state)
      state)))

(defun draw-animation-state (state position &key mirror-x)
  (with-slots (image width height y frame origin) state
    (ge.vg:with-retained-canvas
      (ge.vg:translate-canvas (gamekit:x position) (gamekit:y position))
      (when mirror-x
        (ge.vg:scale-canvas -1 1))
      (ge.vg:translate-canvas (- (gamekit:x origin)) (- (gamekit:y origin)))
      (gamekit:draw-image (gamekit:vec2 0 0)
                          image
                          :origin (gamekit:vec2 (* frame width)
                                                (- (gamekit:image-height image) y))
                          :width width
                          :height height))))

(defclass animated-sprite ()
  ((resource :initarg :resource)
   (state :initarg :state
          :documentation "current animation state")))

(defun make-animated-sprite (resource initial-state)
  (make-instance 'animated-sprite
                 :resource resource
                 :state (make-animation-state resource initial-state)))

(defun draw-animated-sprite (sprite position &key mirror-x)
  (with-slots (state) sprite
    (update-animation-state state)
    (draw-animation-state state position :mirror-x mirror-x)))

(defun animated-sprite-change-animation (sprite animation-name)
  (with-slots (state resource) sprite
    (setf state (make-animation-state resource animation-name))))
