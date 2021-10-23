(in-package :jupiter-candle)

(gamekit:define-image jupiter-candle::welcome "textures/title.png")


(defclass welcome-state () ())


(defmethod render ((this welcome-state))
  (gamekit:draw-image (gamekit:vec2 0 0) 'welcome))

(defmethod act ((this welcome-state))
  nil)

(defmethod activate ((this welcome-state))
  (gamekit:bind-button :space :pressed (lambda () (start-game (gamekit:gamekit))))
  (gamekit:play-sound 'welcome-sound :looped-p t))

(defmethod deactivate ((this welcome-state))
  (gamekit:bind-button :space :pressed nil)
  (gamekit:stop-sound 'welcome-sound))
