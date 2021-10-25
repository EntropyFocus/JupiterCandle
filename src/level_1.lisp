(in-package :jupiter-candle)

;; RANDOM LEVEL GENERATOR

(defparameter *1-object-1-in* 1.1)
(defparameter *2-object-1-in* 1.2)
(defparameter *platform-1-in* 1.7)
(defparameter *moving-1-in* 2.5)
(defparameter *circling-1-in* 2.5)
(defparameter *portal-1-in* 1.2)
(defparameter *jump-pad-1-in* 2.0)

(defun place-object (level-height)
  (if (< (random *platform-1-in*) 1)
      (if (< (random *moving-1-in*) 1)
          (let ((x-offset (random 320))
                (x-speed (+ (random 100) 100))
                (x-range (+ (random 200) 100))
                (clockwise (if (< (random 2) 1) 1 -1)))
            (if (< (random *circling-1-in*) 1)
                (list 'hover-pad :x (lambda (tick) (+ x-offset (* (sin (/ (* clockwise tick) x-speed)) x-range)))
                                 :y (lambda (tick) (+ level-height (* (cos (/ (* clockwise tick) x-speed)) x-range))))
                (list 'hover-pad :x (lambda (tick) (+ x-offset (* (sin (/ tick x-speed)) x-range)))
                                 :y level-height)))
          (list 'platform-s :x (random 640) :y level-height))
      (when (< (random *portal-1-in*) 1)
        (if (< (random *moving-1-in*) 1)
            (list 'jump-pad :x (random 640) :y level-height :force 100)
            (list 'jump-ring :x (random 640) :y level-height)))))

(define-level-section-generator random ()
  (let ((result nil))
    (loop for level-height from 50 below 1000 by 90
          do (when (< (random *1-object-1-in*) 1)
               (push (place-object level-height) result)
               (when (< (random *2-object-1-in*) 1)
                 (push (place-object level-height) result))))
    result))

