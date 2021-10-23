(in-package :jupiter-candle)


(define-level-section-generator railgun
  (let* ((start-x (random 640))
         (result 
           (loop for y from 100 to 260 by 40
                 collect (list 'jump-ring :x start-x :y y))))
    (push (list 'platform-l :x (random 640) :y (+ 1800 (random 800))) result)
    result))

(define-level-section-generator frogger
  (let* ((platforms '(platform-l platform-m platform-s platform-xs jump-ring))
         (random-platform (lambda () (nth (random (length platforms)) platforms))))
    (loop for level from 0 to 6
          collect (let ((x-offset (random 640))
                        (x-speed (+ (random 100) 100))
                        (x-range (+ (random 320) 100)))
                    (list (funcall random-platform)
                          :x (lambda (tick) (+ x-offset (* (sin (/ tick x-speed)) x-range)))
                          :y (+ 70 (* level 80)))))))
