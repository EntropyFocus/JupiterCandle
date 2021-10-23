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

(defun generate-level-section (gamestate level-height generator-name)
  "Generate level elements in GAMESTATE at LEVEL-HEIGHT using the level section
generator GENERATOR-NAME. Returns the y position of the highest generated element."
  (let* ((generator (gethash generator-name *section-generators*))
         (specs (funcall generator))
         (max-y 0))
    (with-slots (elements) gamestate
      (loop for spec in specs
            when spec
            do
            (setf max-y (max max-y (getf (cdr spec) :y)))
            (push (make-element level-height spec) elements)))
    (+ level-height max-y)))

(defun random-generator-name ()
  (let ((idx (random (hash-table-count *section-generators*))))
    (loop for key being the hash-keys of *section-generators*
          for x from 0
          when (= x idx)
          return key)))

;; -------------------------------------------------

(defparameter *static-level*
  (list
   (list 'ground-floor :x 320 :y 10)
   #++(list 'platform-s :x 200 :y (lambda (tick) (+ 100 (* (sin (/ tick 100)) 100))))))

(defun init-level-elements ()
  (loop for spec in *static-level*
        collect (make-element 0 spec)))
