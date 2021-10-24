(in-package :jupiter-candle)


(define-level-section-generator railgun
  (let* ((start-x (random 640))
         (result 
           (loop repeat 6
                 for y from 180 by 40
                 collect (list 'jump-ring :x start-x :y y))))
    (push (list 'platform-m :x (- start-x 200) :y 80) result)
    (push (list 'platform-m :x (+ start-x 200) :y 80) result)
    (push (list 'platform-s :x (random 640) :y (+ 1000 (random 1400))) result)
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

(define-level-section-generator frogger2
  (let* ((platforms '(hover-pad hover-pad hover-pad jump-ring))
         (random-platform (lambda () (nth (random (length platforms)) platforms))))
    (loop for level from 0 to 6
          collect (let ((x-offset (random 640))
                        (x-speed (+ (random 100) 100))
                        (x-range (+ (random 320) 100)))
                    (list (funcall random-platform)
                          :x (lambda (tick) (+ x-offset (* (sin (/ tick x-speed)) x-range)))
                          :y (+ 70 (* level 80)))))))

(defun railgun (start-x start-y &optional (count 3))
  (loop repeat count
        for y from start-y by 40
        collect
        (list 'jump-ring :x start-x :y y)))

(defun build-cone (height &key (exit-x 320))
  (let ((result nil))
    (loop repeat 2
          for x from 10 by 140
          do
          (push (list 'platform-l :x (- x (- 320 exit-x))
                                  :y (+ 100 height (* x 2)) :rotation 1.1)
                result)
          (push (list 'platform-l :x (- 640 x (- 320 exit-x))
                                  :y (+ 100 height (* x 2)) :rotation (- 1.1))
                result))
    (push (list 'platform-l :x (- exit-x 240) :y (+ height 520)) result)
    (push (list 'platform-l :x (+ exit-x 240) :y (+ height 520)) result)
    (setf result (append result (railgun exit-x (+ height 490) 3)))
    result))

(define-level-section-generator cone
  (let* ((first-exit-x (random 640))
         (result (loop repeat 2
                       for y from 0 by 2000
                       for exit-x = first-exit-x then (random 640)
                       nconcing (build-cone y :exit-x exit-x))))
    (setf result (append result (railgun first-exit-x 100 3)))
    (push (list 'platform-s :x (random 640) :y 4200) result)
    result))

(define-level-section-generator tutorial
  (list
   (list 'text :text "Left, Right to move" :x 30 :y 80 :font-size 20)
   (list 'text :text "Up to jump" :x 300 :y 170 :font-size 20)
   (list 'platform-m :x 500 :y 140)
   (list 'platform-s :x 190 :y 200)
   (list 'platform-xs :x 30 :y 240)
   (list 'platform-l :x 400 :y 380)
   (list 'text :text "C to dash" :x 80 :y 380 :font-size 20)
   (list 'text :text "Have fun!" :x 320 :y 430 :font-size 30)
   (list 'jump-pad :x 660 :y 12 :force 100)))

