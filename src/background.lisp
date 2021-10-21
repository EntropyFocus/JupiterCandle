(in-package :jupiter-candle)

(gamekit:register-resource-package :jupiter-candle (asdf:system-relative-pathname :jupiter-candle "assets/"))

(gamekit:define-image jupiter-candle::bg-1 "textures/tilemap1.png")

(defun draw-background (y-offset)
  (gamekit:draw-image (gamekit:vec2 0 0) 'bg-1))
