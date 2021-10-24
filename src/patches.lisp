(in-package :gamekit)

(defun (setf sound-pitch) (pitch sound-id)
  (let ((source (resource-by-id sound-id)))
    (setf (ge.snd:audio-pitch source) pitch)))

(defun sound-pitch (sound-id)
  (let ((source (resource-by-id sound-id)))
    (ge.snd:audio-pitch source)))

(defun (setf sound-gain) (gain sound-id)
  (let ((source (resource-by-id sound-id)))
    (setf (ge.snd:audio-gain source) gain)))

(defun sound-gain (sound-id)
  (let ((source (resource-by-id sound-id)))
    (ge.snd:audio-gain source)))

(export 'sound-pitch)
(export 'sound-gain)
