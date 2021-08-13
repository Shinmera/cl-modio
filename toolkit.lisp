#|
 This file is a part of cl-modio
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.modio)

(defun detect-platform ()
  #+windows :windows
  #+linux :linux
  #+darwin :mac)

(defun unix-timestamp (&optional (time (get-universal-time)))
  (- time (encode-universal-time 0 0 0 1 1 1970 0)))

(defun expiry-timestamp (&optional (time (get-universal-time)))
  (+ time 31536000))
