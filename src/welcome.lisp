(in-package :jupiter-candle)

(gamekit:define-image jupiter-candle::welcome "textures/title.png")

(defparameter *welcome-text*
  (uiop:split-string
   "The sun is turning into a red giant
threatening Earth and all life on it.
Humanity has created a giant fusion
engine to push Jupiter around and use
it's gravitational pull to move Earth
to safety.

After a violent solar flare, the
damaged engine is bound to crash into
the gas planet. Can the few remaining
crew at the bottom reach the control
center in time to save us all?

[arrow keys] to move and jump

Press SPACE to start the game"
   :separator '(#\Newline)))


(defclass welcome-state () ())


(defmethod render ((this welcome-state))
  (gamekit:draw-image (gamekit:vec2 0 0) 'welcome)
  (loop for line in *welcome-text*
        for y from 355 downto 0 by 16
        for x = 320
        do
        (gamekit:draw-text line (gamekit:vec2 (1+ x) (- y 0.9)) :fill-color (gamekit:vec4 0 0 0 1))
        (gamekit:draw-text line (gamekit:vec2 x y) :fill-color (gamekit:vec4 0.8 0 0 1))))

(defmethod act ((this welcome-state))
  nil)

(defmethod activate ((this welcome-state))
  (gamekit:bind-button :space :pressed (lambda () (start-game (gamekit:gamekit))))
  (gamekit:play-sound 'welcome-sound :looped-p t))

(defmethod deactivate ((this welcome-state))
  (gamekit:bind-button :space :pressed nil)
  (gamekit:stop-sound 'welcome-sound))
