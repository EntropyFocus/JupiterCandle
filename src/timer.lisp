(in-package :jupiter-candle)

;; Code from https://gitlab.com/decent-username/dmomd/-/blob/master/src/timer-utils.lisp

(defvar *timers* nil)

;;;; Timers
;;;;----------------------------------------------------------------------------
(defun now ()
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun timer-time (timer) (car timer))
(defun timer-handler (timer) (cdr timer))


(defun insert-timer (timer timers)
  "Inserts `timer' into the `timers' list, while making sure `timers' stays sorted.
Doesn't modify the original `timers' list."
  (if (endp timers)
      (list timer)
      (if (> (timer-time (first timers))
             (timer-time timer))
          (cons timer timers)
          (cons (first timers)
                (insert-timer timer (rest timers))))))


(defun add-timer (target handler)
  (setf *timers* (insert-timer (cons target handler) *timers*)))


(defun process-timers ()
  ;; this does not take advantage of *timers* being sorted.  For robustness. :D
  (let ((expired (find-if (lambda (timer)
                            (>= (now)
                                (car timer)))
                          *timers*)))
    (when expired
      (funcall (cdr expired))
      (setf *timers* (delete expired *timers*))
      (process-timers))))
