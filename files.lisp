#|
 This file is a part of cl-modio
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.modio)

(define-list-endpoint* (games/mods/files modfile "games/~a/mods/~a/files" game mod)
  (modfile :parameter :id :update id)
  (date-added :update unix-timestamp)
  (date-scanned :update unix-timestamp)
  (virus-status :update virus-status-id)
  (virus-positive :update virus-positive-id)
  (file-size :parameter :filesize)
  (file-hash :parameter :filehash)
  (file-name :parameter :filename)
  version
  changelog
  metadata-blob)

(define-edit-endpoint :get (games/mods/files/get modfile "games/~a/mods/~a/files/~a" game mod file))

(define-endpoint (games/mods/files/add (format nil "games/~a/mods/~a/files" (id game) (id mod)) :post)
    (game mod file &key version changelog active file-hash file-name metadata-blob)
  (labels ((call (filedata)
             (let ((data (request :filedata filedata :version version :changelog changelog
                                  :active active :filehash file-hash :metadata-blob metadata-blob)))
               (cache-object client 'modfile data)))
           (zip (file)
             (let ((temp (make-temp-file :type "zip")))
               (org.shirakumo.zippy:compress-zip file temp)
               (unwind-protect (call temp)
                 (delete-file temp)))))
    (etypecase file
      ((or pathname string)
       (cond ((or (wild-pathname-p file)
                  (not (equalp "zip" (pathname-type file))))
              (zip file))
             (T
              (call file))))
      (org.shirakumo.zippy:zip-file
       (zip file))
      ((vector (unsigned-byte 8))
       (let ((entries (make-array 1)))
         (setf (aref entries 0) (make-instance 'zip-file :content file :file-name file-name))
         (zip (make-instance 'org.shirakumo.zippy:zip-file :entries entries :comment "Created with Zippy")))))))

(define-edit-endpoint :put (games/mods/files/edit modfile "games/~a/mods/~a/files/~a" game mod file)
  version
  changelog
  active
  metadata-blob)

(define-edit-endpoint :delete (games/mods/files/edit NIL "games/~a/mods/~a/files/~a" game mod file))

(defmethod download-modfile ((file modfile) target &key (if-exists :supersede)
                                                        (if-does-not-exist :create))
  (let* ((download (download file))
         (target (merge-pathnames target (file-name file))))
    (flet ((download ()
             (with-open-stream (input (request *client* (binary-url download) :parse NIL))
               (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8)))
                     (total 0))
                 (declare (dynamic-extent buffer))
                 (with-open-file (output target :direction :output
                                                :element-type '(unsigned-byte 8)
                                                :if-exists :supersede)
                   (unwind-protect
                        (loop for read = (read-sequence buffer input)
                              until (= 0 read)
                              do (write-sequence buffer output)
                                 (incf total read))
                     (when (/= total (file-size file))
                       (cerror "Ignore the discrepancy." "File size not as expected!"))))))))
      (cond ((not (probe-file target))
             (ecase if-does-not-exist
               (:create
                (download))
               (:error
                (error "Download does not exist."))
               ((NIL)
                NIL)))
            (T
             (ecase if-exists
               (:supersede
                (if (< (file-write-date target) (date-added file))
                    (download)
                    target))
               (:overwrite
                (download))
               (:return
                 target)
               ((NIL)
                NIL)))))))

(defmethod extract-modfile ((file modfile) target &key (if-no-cache :create)
                                                       (if-exists :supersede))
  (let ((cache (download-modfile file T :if-exists :supersede :if-does-not-exist if-no-cache)))
    (when cache
      (when (probe-file target)
        (ecase if-exists
          (:supersede
           (delete-directory target))
          (:overwrite)
          (:error
           (error "Target exists."))
          ((NIL)
           (return-from extract-modfile NIL))))
      (ensure-directories-exist target)
      (org.shirakumo.zippy:extract-zip cache target :if-exists :supersede))))
