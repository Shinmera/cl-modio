#|
 This file is a part of cl-modio
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.modio)

(defclass simple-client (client)
  ((default-game-id :initform (error "DEFAULT-GAME-ID required."))
   (mod-directory :initarg :mod-directory :initform (error "MOD-DIRECTORY required.") :accessor mod-directory)))

(defmethod modlist-file ((client simple-client))
  (merge-pathnames ".modlist.dat" (mod-directory client)))

(defmethod modfile-cache-directory ((client simple-client))
  (merge-pathnames "cache/" (mod-directory client)))

(defmethod load-local-modlist ((client simple-client))
  (with-open-file (stream (modlist-file client) :direction :input)
    (loop for line = (read-line stream NIL NIL)
          while line
          collect (multiple-value-bind (mod-id end) (parse-integer line)
                    (multiple-value-bind (modfile-id end) (parse-integer line :start end)
                      (declare (ignore end))
                      (games/mods/files/get (default-game-id client) mod-id modfile-id))))))
