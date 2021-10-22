(in-package :jupiter-candle)


(define-level-section-generator railgun
  (let* ((start-x (random 640))
         (result 
           (loop for y from 40 to 200 by 40
                 collect (list :jump-ring :x start-x :y y :width 70))))
    (push (list :floor :x (random 640) :y 4000 :width 200) result)
    result))
