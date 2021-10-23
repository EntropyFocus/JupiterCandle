(in-package :jupiter-candle)


(define-level-section-generator railgun
  (let* ((start-x (random 640))
         (result 
           (loop for y from 100 to 260 by 40
                 collect (list 'jump-ring :x start-x :y y))))
    (push (list 'platform-l :x (random 640) :y (+ 1800 (random 800))) result)
    result))
