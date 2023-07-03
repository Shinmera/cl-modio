(in-package #:org.shirakumo.fraf.modio)

(define-condition modfile-error (error modio-condition)
  ((modfile :initarg :modfile :reader modfile)
   (target :initarg :target :reader target)))

(define-condition download-corrupted (modfile-error)
  ((file-size :initarg :file-size :reader file-size))
  (:report (lambda (c s) (format s "Download of~%  ~a~%at~%  ~a~%was ~d bytes long, but expected ~d."
                                 (modfile c) (target c) (file-size c) (file-size (modfile c))))))

(define-condition target-does-not-exist (modfile-error)
  ()
  (:report (lambda (c s) (format s "The target for~%  ~a~%is not cached at~%  ~a"
                                 (modfile c) (target c)))))

(define-condition target-already-exists (modfile-error)
  ()
  (:report (lambda (c s) (format s "The target for~%  ~a~%already exists at~%  ~a"
                                 (modfile c) (target c)))))

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

(define-edit-endpoint :delete (games/mods/files/delete NIL "games/~a/mods/~a/files/~a" game mod file))

(defmethod download-modfile ((file modfile) target &key (if-exists :supersede)
                                                        (if-does-not-exist :create))
  (let* ((download (download file))
         (target (merge-pathnames target (file-name file))))
    (flet ((download ()
             (with-open-stream (input (request *client* (binary-url download) :parse NIL :prepend-base NIL))
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
                       (cerror "Ignore the discrepancy" 'download-corrupted
                               :modfile file :file-size total))))
                 target))))
      (cond ((not (probe-file target))
             (ecase if-does-not-exist
               (:create
                (download))
               (:error
                (error 'target-does-not-exist :modfile file :target target))
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
               (:error
                (error 'target-already-exists :modfile file :target target))
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
           (error 'target-already-exists :modfile file :target target))
          ((NIL)
           (return-from extract-modfile NIL))))
      (ensure-directories-exist target)
      (org.shirakumo.zippy:extract-zip cache target :if-exists :supersede))))
