(in-package :jupiter-candle)

;; Code adapted
;; from https://gitlab.com/decent-username/dmomd/-/blob/master/src/timer-utils.lisp

(defstruct timerset
  (timers))

(defvar *default-timerset* (make-timerset))

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


(defun add-timer (target handler &optional (timerset *default-timerset*))
  (setf (timerset-timers timerset) (insert-timer (cons target handler)
                                                 (timerset-timers timerset))))

(defun cancel-timers (&optional (timerset *default-timerset*))
  "Clear remaining timers without executing them"
  (setf (timerset-timers timerset) nil))

(defun process-timers (&optional (timerset *default-timerset*))
  ;; this does not take advantage of *default-timerset* being sorted.  For robustness. :D
  (let ((expired (find-if (lambda (timer)
                            (>= (now)
                                (car timer)))
                          (timerset-timers timerset))))
    (when expired
      (funcall (cdr expired))
      (setf (timerset-timers timerset) (delete expired (timerset-timers timerset)))
      (process-timers timerset))))
