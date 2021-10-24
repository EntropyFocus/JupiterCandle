(in-package :jupiter-candle)

(gamekit:register-resource-package :jupiter-candle (asdf:system-relative-pathname :jupiter-candle "assets/"))

(gamekit:define-font jupiter-candle::hud-font "fonts/vice-versus.otf")
(gamekit:define-font jupiter-candle::menu-font "fonts/hemihead.ttf")

(gamekit:define-sound jupiter-candle::game-sound "sound/Nihilore-Truth-and-Justification-07-A-Tremendous-Thing.ogg")
(gamekit:define-sound jupiter-candle::welcome-sound "sound/Metroidvania Mini Pack - Ambiance_1.wav")


(gamekit:define-sound jupiter-candle::dash-sound "sound/SFX_dash.wav")
(gamekit:define-sound jupiter-candle::jump-sound "sound/SFX_jump.wav")
(gamekit:define-sound jupiter-candle::bump-sound "sound/SFX_bump.wav")
(gamekit:define-sound jupiter-candle::portal-sound "sound/SFX_portal.wav")


(defun init-sounds ()
  (setf (gamekit:sound-gain 'dash-sound) 2.0)
  (setf (gamekit:sound-gain 'jump-sound) 2.0)
  (setf (gamekit:sound-gain 'bump-sound) 2.0)
  (setf (gamekit:sound-gain 'portal-sound) 2.0)
  (setf (gamekit:sound-gain 'game-sound) 0.5))
