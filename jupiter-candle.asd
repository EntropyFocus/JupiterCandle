(defsystem "jupiter-candle"
  :version "0.1.0"
  :author "entropic games"
  :license ""
  :depends-on ("trivial-gamekit" "cl-bodge/physics" "cl-bodge/physics/2d" "alexandria")
  :components ((:module "src"
                :components
                ((:file "packages")
                 (:file "timer")
                 (:file "assets")
                 (:file "welcome")
                 (:file "sprite")
                 (:file "background")
                 (:file "element")
                 (:file "player")
                 (:file "level")
                 (:file "level_1")
                 (:file "level_2")
                 (:file "gamestate")
                 (:file "main")
                 (:file "ui"))))
  :description ""
  :in-order-to ((test-op (test-op "JupiterCandle/tests"))))
