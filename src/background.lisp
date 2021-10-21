(in-package :jupiter-candle)

(gamekit:register-resource-package :jupiter-candle (asdf:system-relative-pathname :jupiter-candle "assets/"))

(gamekit:define-image jupiter-candle::bg-1 "textures/tilemap1.png")
(gamekit:define-image jupiter-candle::bg-2 "textures/tilemap2.png")
(gamekit:define-image jupiter-candle::bg-3 "textures/tilemap3.png")
(gamekit:define-image jupiter-candle::bg-4 "textures/tilemap4.png")
(gamekit:define-image jupiter-candle::bg-5 "textures/tilemap5.png")

(defparameter *bg-image-height* 640)
(defparameter *bg-images* '(bg-1 bg-2 bg-3 bg-4 bg-5))

(defun draw-background (y-offset)
  (let ((offset (- (mod (- y-offset) *bg-image-height*))))
    (gamekit:with-pushed-canvas ()    
      (gamekit:translate-canvas 0 offset)
      (gamekit:draw-image (gamekit:vec2 0 0) 'bg-1)
      (gamekit:draw-image (gamekit:vec2 0 640) 'bg-2))))

