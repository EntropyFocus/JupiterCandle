(in-package :jupiter-candle)

(gamekit:define-image jupiter-candle::welcome "textures/title.png")
(gamekit:define-image jupiter-candle::welcome2 "textures/title2.png")

(defparameter *welcome-text*
  (uiop:split-string
   "The sun is turning into a red giant,
threatening Earth and all life on it.
Humanity has created a giant fusion
engine to push Jupiter around and
use its gravitational pull to move
Earth to safety.

After a violent solar flare the damaged
engine is bound to crash into the gas 
planet.
Can the few remaining crew at 
the bottom reach the control center 
in time to save us all?



(created (CL) '(Entropic Games))

= Philipp Sieweck
= Gereon Bartel



external assets
(details in readme.txt)

Music:
- This menu music - Fred Patry
- In game music - Nihilore

SFX:
- All from freesound.org

Graphics:
- Player figure - deadrevolver
- Tile background - zingot

Fonts:
- Vice Versus - Chequered Ink
- Hemi Head 426 - Typodermic Fonts



Thanks for playing!
"
   :separator '(#\Newline)))

(defparameter *text-start-y* -810)
(defparameter *text-end-y* 430)

(defparameter *font-color* (gamekit:vec4 1 1 0.8 1))
(defparameter *shadow-color* (gamekit:vec4 0.2 0.2 0 1))

(defclass welcome-state ()
  ((y-pos :initform *text-start-y*)))


(defmethod render ((this welcome-state))
  (gamekit:draw-image (gamekit:vec2 0 0) 'welcome)
  (with-slots (y-pos) this
    (loop for line in *welcome-text*
        for y from 880 downto 0 by 20
        for x = 280
        do
          (gamekit:draw-text line (gamekit:vec2 (+ x 1) (+ y-pos (- y 1))) :fill-color *shadow-color* :font (gamekit:make-font 'menu-font 20))
          (gamekit:draw-text line (gamekit:vec2 (- x 1) (+ y-pos (+ y 1))) :fill-color *shadow-color* :font (gamekit:make-font 'menu-font 20))
          (gamekit:draw-text line (gamekit:vec2 x (+ y-pos y)) :fill-color *font-color* :font (gamekit:make-font 'menu-font 20))))

  (gamekit:draw-image (gamekit:vec2 0 0) 'welcome2)

  (if t
      (progn
        (gamekit:draw-text "press SPACE to go save the" (gamekit:vec2 109 76) :fill-color *shadow-color* :font (gamekit:make-font 'menu-font 24))
        (gamekit:draw-text "press SPACE to go save the" (gamekit:vec2 112 73) :fill-color *shadow-color* :font (gamekit:make-font 'menu-font 24))
        (gamekit:draw-text "press SPACE to go save the" (gamekit:vec2 110 75) :fill-color *font-color* :font (gamekit:make-font 'menu-font 24)))
      (progn
        (gamekit:draw-text "loading..." (gamekit:vec2 109 76) :fill-color *shadow-color* :font (gamekit:make-font 'menu-font 24))
        (gamekit:draw-text "loading..." (gamekit:vec2 112 73) :fill-color *shadow-color* :font (gamekit:make-font 'menu-font 24))
        (gamekit:draw-text "loading..." (gamekit:vec2 110 75) :fill-color *font-color* :font (gamekit:make-font 'menu-font 24))))

        
  (gamekit:draw-text "Jupiter Candle" (gamekit:vec2 129 21) :fill-color *shadow-color* :font (gamekit:make-font 'menu-font 76))
  (gamekit:draw-text "Jupiter Candle" (gamekit:vec2 135 15) :fill-color *shadow-color* :font (gamekit:make-font 'menu-font 76))
  (gamekit:draw-text "Jupiter Candle" (gamekit:vec2 130 20) :fill-color *font-color* :font (gamekit:make-font 'menu-font 76)))

(defmethod act ((this welcome-state))
  (with-slots (y-pos) this
    (incf y-pos 0.5)
    (when (> y-pos *text-end-y*)
      (decf y-pos *text-end-y*)
      (incf y-pos *text-start-y*))))

(defmethod activate ((this welcome-state))
  (gamekit:bind-button :space :pressed (lambda () (start-game (gamekit:gamekit))))
  (gamekit:play-sound 'welcome-sound :looped-p t))

(defmethod deactivate ((this welcome-state))
  (gamekit:bind-button :space :pressed nil)
  (gamekit:stop-sound 'welcome-sound))
