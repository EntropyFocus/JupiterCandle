(in-package :jupiter-candle)

(defvar *production-p* nil)

(defvar *number-of-loaded-resources* 0)
(defparameter *requested-resources* (make-hash-table))

(defun asset-path (pathname)
  (if *production-p*
      (merge-pathnames pathname "assets/")
      (asdf:system-relative-pathname :jupiter-candle (merge-pathnames pathname "assets/"))))

(defmacro register-font (name path)
  `(when (not (gethash ',name *requested-resources*))
     (setf (gethash ',name *requested-resources*) :requested)
     (gamekit:define-font ,name (asset-path ,path))
     (when (gamekit:gamekit)
       (gamekit:prepare-resources ',name))))

(defmacro register-image (name path)
  `(when (not (gethash ',name *requested-resources*))
     (setf (gethash ',name *requested-resources*) :requested)
     (gamekit:define-image ,name (asset-path ,path))
     (when (gamekit:gamekit)
       (gamekit:prepare-resources ',name))))

(defmacro register-sound (name path)
  `(when (not (gethash ',name *requested-resources*))
     (setf (gethash ',name *requested-resources*) :requested)
     (gamekit:define-sound ,name (asset-path ,path))
     (when (gamekit:gamekit)
       (gamekit:prepare-resources ',name))))

;(gamekit:register-resource-package :jupiter-candle (asdf:system-relative-pathname :jupiter-candle "assets/"))

(defun load-main-menu-resources ()
  (register-font jupiter-candle::menu-font "fonts/hemihead.ttf")
  (register-sound jupiter-candle::welcome-sound "sound/Metroidvania Mini Pack - Ambiance_1.wav")
  (register-image jupiter-candle::welcome "textures/title.png")
  (register-image jupiter-candle::welcome2 "textures/title2.png"))

(defun load-game-resources ()
  (register-font jupiter-candle::hud-font "fonts/vice-versus.otf")
  (register-sound jupiter-candle::game-sound "sound/Nihilore-Truth-and-Justification-07-A-Tremendous-Thing.ogg")
  (register-sound jupiter-candle::dash-sound "sound/SFX_dash.wav")
  (register-sound jupiter-candle::jump-sound "sound/SFX_jump.wav")
  (register-sound jupiter-candle::bump-sound "sound/SFX_bump.wav")
  (register-sound jupiter-candle::portal-sound "sound/SFX_portal.wav")
  (register-sound jupiter-candle::aaah-sound "sound/SFX_aah.wav")
  (register-sound jupiter-candle::weee-sound "sound/SFX_wee.wav")
  (register-sound jupiter-candle::hit-ground-sound "sound/SFX_hit_ground.wav")

  (register-image jupiter-candle::ground-floor "textures/ground_floor.png")
  (register-image jupiter-candle::platform-l "textures/platform_l.png")
  (register-image jupiter-candle::platform-m "textures/platform_m.png")
  (register-image jupiter-candle::platform-s "textures/platform_s.png")
  (register-image jupiter-candle::platform-xs "textures/platform_xs.png")

  (register-image jupiter-candle::jump-pad "textures/jump_pad.png")
  (register-image jupiter-candle::trampoline-pad "textures/trampoline_pad.png")
  (register-image jupiter-candle::moving-platform-anim "textures/moving-platform.png")
  
  (register-image jupiter-candle::bg-1 "textures/tilemap1.png")
  (register-image jupiter-candle::bg-2 "textures/tilemap2.png")
  (register-image jupiter-candle::bg-3 "textures/tilemap3.png")
  (register-image jupiter-candle::bg-4 "textures/tilemap4.png")
  (register-image jupiter-candle::bg-5 "textures/tilemap5.png")
  (register-image jupiter-candle::bg-6 "textures/tilemap6.png")
  (register-image jupiter-candle::bg-7 "textures/tilemap7.png")
  (register-image jupiter-candle::bg-8 "textures/tilemap8.png")
  (register-image jupiter-candle::bg-9 "textures/tilemap9.png")

  (register-image jupiter-candle::portal-anim "textures/Portal.png")

  (register-image jupiter-candle::layer-2-bg "textures/background.png")

  (register-image jupiter-candle::player-anim "textures/player.png"))

(export 'switch-to-production)
(defun switch-to-production ()
  "TODO"
  (setq *production-p* t)
  (setq *number-of-loaded-resources* 0)
  (setq *requested-resources* (make-hash-table)))
