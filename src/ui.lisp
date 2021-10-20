(in-package :jupiter-candle)

(ge.ui:defpanel (my-window
                 (:title "Hello Wfadforld UI")
                 (:origin 200 50)
                 (:width 400)
                 (:height 300)
                 (:options :movable :resizable :minimizable :scrollable :closable)
                 (:style :panel-padding (gamekit:vec2 10 10)))
  (ge.ui:label :text "Nested:")
  (ge.ui:horizontal-layout
   (ge.ui:radio-group
    (ge.ui:radio :label "My Option")
    (ge.ui:radio :label "Yeah" :activated t))))
