(in-package :jupiter-candle)

(gamekit:register-resource-package :jupiter-candle (asdf:system-relative-pathname :jupiter-candle "assets/"))

(gamekit:define-image jupiter-candle::bg-1 "textures/tilemap1.png")
(gamekit:define-image jupiter-candle::bg-2 "textures/tilemap2.png")
(gamekit:define-image jupiter-candle::bg-3 "textures/tilemap3.png")
(gamekit:define-image jupiter-candle::bg-4 "textures/tilemap4.png")
(gamekit:define-image jupiter-candle::bg-5 "textures/tilemap5.png")

(defparameter *number-of-bg-images* 5)
(defparameter *bg-image-height* 640)
(defparameter *bg-images* (vector 'bg-1 'bg-2 'bg-3 'bg-4 'bg-5))

(defparameter *bg-random-state* (make-random-state))

(defparameter *parallax-factor* 1.7)

(defun draw-background (y-offset)
  (let* ((parallax-offset (/ y-offset *parallax-factor*))
         (offset (ceiling  (- (mod (- parallax-offset) *bg-image-height*))))
         (index (floor (/ (- parallax-offset) *bg-image-height*))))
    (gamekit:with-pushed-canvas ()
      (gamekit:translate-canvas 0 offset)
      (let ((rs (make-random-state *bg-random-state*)))
        (dotimes (n index)
          (random *number-of-bg-images* rs))
        (let ((bg-index (random *number-of-bg-images* rs))
              (bg-index-2 (random *number-of-bg-images* rs)))
          (gamekit:draw-image (gamekit:vec2 0 0) (aref *bg-images* bg-index))
          (gamekit:draw-image (gamekit:vec2 0 640) (aref *bg-images* bg-index-2)))))))

