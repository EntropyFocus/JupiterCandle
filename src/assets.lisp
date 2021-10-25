(in-package :jupiter-candle)

(defvar *number-of-loaded-resources* 0)
(defparameter *requested-resources* (make-hash-table))

(defun asset-path (pathname)
  (asdf:system-relative-pathname :jupiter-candle (merge-pathnames pathname "assets/")))

(defmacro register-font (name path)
  `(when (not (gethash ,name *requested-resources*))
     (setf (gethash ,name *requested-resources*) :requested)
     (gamekit:define-font ,name (asset-path ,path))
     (when (gamekit:gamekit)
       (gamekit:prepare-resources ,name))))

(defmacro register-image (name path)
  `(when (not (gethash ,name *requested-resources*))
     (setf (gethash ,name *requested-resources*) :requested)
     (gamekit:define-image ,name (asset-path ,path))
     (when (gamekit:gamekit)
       (gamekit:prepare-resources ,name))))

(defmacro register-sound (name path)
  `(when (not (gethash ,name *requested-resources*))
     (setf (gethash ,name *requested-resources*) :requested)
     (gamekit:define-sound ,name (asset-path ,path))
     (when (gamekit:gamekit)
       (gamekit:prepare-resources ,name))))

;(gamekit:register-resource-package :jupiter-candle (asdf:system-relative-pathname :jupiter-candle "assets/"))

(defun load-main-menu-resources ()
  (register-font :menu-font "fonts/hemihead.ttf")
  (register-sound :welcome-sound "sound/Metroidvania Mini Pack - Ambiance_1.wav")
  (register-image :welcome "textures/title.png")
  (register-image :welcome2 "textures/title2.png"))

(defun load-game-resources ()
  (register-font :hud-font "fonts/vice-versus.otf")
  (register-sound :game-sound "sound/Nihilore-Truth-and-Justification-07-A-Tremendous-Thing.ogg")
  (register-sound :dash-sound "sound/SFX_dash.wav")
  (register-sound :jump-sound "sound/SFX_jump.wav")
  (register-sound :bump-sound "sound/SFX_bump.wav")
  (register-sound :portal-sound "sound/SFX_portal.wav")
  (register-sound :aaah-sound "sound/SFX_aah.wav")
  (register-sound :weee-sound "sound/SFX_wee.wav")
  (register-sound :hit-ground-sound "sound/SFX_hit_ground.wav")

  (register-image :ground-floor "textures/ground_floor.png")
  (register-image :platform-l "textures/platform_l.png")
  (register-image :platform-m "textures/platform_m.png")
  (register-image :platform-s "textures/platform_s.png")
  (register-image :platform-xs "textures/platform_xs.png")

  (register-image :jump-pad "textures/jump_pad.png")
  (register-image :trampoline-pad "textures/trampoline_pad.png")
  (register-image :moving-platform-anim "textures/moving-platform.png")
  
  (register-image :bg-1 "textures/tilemap1.png")
  (register-image :bg-2 "textures/tilemap2.png")
  (register-image :bg-3 "textures/tilemap3.png")
  (register-image :bg-4 "textures/tilemap4.png")
  (register-image :bg-5 "textures/tilemap5.png")
  (register-image :bg-6 "textures/tilemap6.png")
  (register-image :bg-7 "textures/tilemap7.png")
  (register-image :bg-8 "textures/tilemap8.png")
  (register-image :bg-9 "textures/tilemap9.png")

  (register-image :portal-anim "textures/Portal.png")

  (register-image :layer-2-bg "textures/background.png")

  (register-image :player-anim "textures/player.png"))
