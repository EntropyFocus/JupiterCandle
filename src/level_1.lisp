(in-package :jupiter-candle)

;; RANDOM LEVEL GENERATOR

(defparameter *1-object-1-in* 1.2)
(defparameter *2-object-1-in* 1.2)
(defparameter *platform-1-in* 1.7)
(defparameter *portal-1-in* 1.2)

(defun place-object (level-height)
  (if (< (random *platform-1-in*) 1)       
      (list :floor :x (random 900) :y level-height :width 100)
      (when (< (random *portal-1-in*) 1)
        (list :jump-ring :x (random 900) :y level-height))))

(define-level-section-generator random-level ()
  (let ((result nil))
    (loop for level-height from 50 below 1000 by 50
          do (when (< (random *1-object-1-in*) 1)
               (push (place-object level-height) result)
               (when (< (random *2-object-1-in*) 1)
                 (push (place-object level-height) result))))
    result))

