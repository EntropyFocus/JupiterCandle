(in-package :jupiter-candle)

;;; Elements for collision


(defvar *section-generators* (make-hash-table))

(defmacro define-level-section-generator (name &body body)
  "Define a level section generator named NAME. A generator returns a list of
element specifications. A specification for an element looks like this:

    (TYPE &key X Y &allow-other-keys)

Examples for TYPE are :FLOOR or :JUMP-RING. Depending on TYPE, a specification
can contain other parameters, like :WIDTH.

X and Y specify the position within the level section."
  `(progn
     (setf (gethash ',name *section-generators*) (lambda () ,@body))))

(defun make-element (level-height spec)
  (alexandria:destructuring-ecase spec
    ((:jump-ring &key (x 0) (y 0))
     (make-instance 'jump-ring-element
                    :position (gamekit:vec2 x (+ level-height y))))
    ((:floor &key (x 0) (y 0) (width 10))
     (make-instance 'floor-element
                    :position (gamekit:vec2 x (+ level-height y)) :width width))))

(defun generate-level-section (gamestate level-height generator-name)
  (let* ((generator (gethash generator-name *section-generators*))
         (specs (funcall generator)))
    (with-slots (elements) gamestate
      (dolist (spec specs)
        (push (make-element level-height spec) elements)))))

(defvar *level-height* 0)

(defun update-level (gamestate desired-height)
  "Generate level elements up to DESIRED-HEIGHT. To be done."
  ())

;; -------------------------------------------------

(defparameter *static-level*
  '((:floor :x 320 :y 10 :width 640)
    (:floor :x 340 :y 40 :width  40)
    (:floor :x 370 :y 60 :width  40)
    (:floor :x 150 :y 90 :width  40)
    (:floor :x 340 :y 190 :width  40)
    (:floor :x 50 :y 30 :width 30)))

(define-level-section-generator static ()
  *static-level*)

(defun init-level-elements ()
  (let ((result nil))
    (loop :for name in '(static random)
          :for generator = (gethash name *section-generators*)
          :for specs = (funcall generator)
          :do (loop for spec in specs
                    when spec
                    do (push (make-element 0 spec) result)))
    result))
