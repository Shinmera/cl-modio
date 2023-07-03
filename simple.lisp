(in-package #:org.shirakumo.fraf.modio)

(defclass simple-client (client)
  ((default-game-id :initform (error "DEFAULT-GAME-ID required."))
   (mods-directory :initarg :mods-directory :initform (error "MODS-DIRECTORY required.") :accessor mods-directory)))

(defmethod modlist-file ((client simple-client))
  (merge-pathnames ".modlist.dat" (mods-directory client)))

(defmethod modfile-cache-directory ((client simple-client))
  (merge-pathnames "cache/" (mods-directory client)))

(defmethod mod-directory ((client simple-client) mod)
  (merge-pathnames (make-pathname :directory `(:relative ,(princ-to-string (id mod))))
                   (mods-directory client)))

(defun normalize-modlist-entry (entry)
  (destructuring-bind (mod-id &key file-id version active freeze) entry
    (list mod-id :file-id file-id :version version :active active :freeze freeze)))

(defmethod load-local-modlist ((client simple-client))
  (with-open-file (stream (modlist-file client) :direction :input)
    (with-standard-io-syntax
      (loop with *read-eval* = NIL
            for read = (read stream NIL '#1=#:no-value)
            until (eq read '#1#)
            collect (normalize-modlist-entry read)))))

(defmethod write-local-modlist ((client simple-client) modlist)
  (with-open-file (stream (modlist-file client) :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (let ((*print-case* :downcase))
        (format stream ";;; mod.io modlist")
        (dolist (entry modlist modlist)
          (terpri stream)
          (write entry :stream stream))))))

(defmethod load-remote-modlist ((client simple-client))
  (loop for mod in (me/subscribed client :game (default-game-id client))
        collect (normalize-modlist-entry (list (id mod)))))

(defmethod download-modfile ((file modfile) (client simple-client) &rest args)
  (apply #'download-modfile
         file
         (make-pathname :name (id file) :type "zip"
                        :defaults (modfile-cache-directory client))
         args))

(defmethod extract-modfile ((file modfile) (client simple-client) &rest args)
  (apply #'extract-modfile
         file
         (mod-directory client (mod-id file))
         args))

(defmethod download-modfile ((file modfile) (default (eql T)) &rest args)
  (apply #'download-modfile file *client* args))

(defmethod extract-modfile ((file modfile) (default (eql T)) &rest args)
  (apply #'extract-modfile file *client* args))

(defmethod find-modfile ((client simple-client) mod-id &key file-id version)
  (or (cond (file-id (games/mods/files/get client (default-game-id client) mod-id file-id))
            (version (first (games/mods/files client (default-game-id client) mod-id :version version :sort `((date-added . :desc)) :end 1)))
            (T (first (games/mods/files client (default-game-id client) mod-id :sort `((date-added . :desc)) :end 1))))
      (restart-case (error 'modfile-not-found :endopint "games/mods/files")
        (use-latest ()
          :report "Use the latest modfile version instead."
          :test (lambda (c) (declare (ignore c)) (or file-id version))
          (find-modfile client mod-id))
        (use-value (value)
          :report "Use a provided modfile instead."
          value))))

(defmethod sync-modlist-from-remote ((client simple-client) &key (if-does-not-exist :create)
                                                                 (if-exists :deactivate))
  (let* ((local (load-local-modlist client))
         (remote (load-remote-modlist client))
         (to-download (set-difference remote local :key #'first))
         (to-delete (set-difference local remote :key #'first)))
    (dolist (entry to-download)
      (destructuring-bind (mod-id &key file-id version &allow-other-keys) entry
        (ecase if-does-not-exist
          (:create
           (let ((file (find-modfile client mod-id :file-id file-id :version version)))
             (extract-modfile file client)
             (push (normalize-modlist-entry (list mod-id :file-id (id file) :version (version file) :active T))
                   local)))
          (:error
           (cerror "Ignore the missing mod" "Mod is not present locally."))
          ((NIL)))))
    (dolist (entry to-delete)
      (destructuring-bind (mod-id &key freeze &allow-other-keys) entry
        (unless freeze
          (ecase if-exists
            (:delete
             (delete-directory (mod-directory client mod-id))
             (setf local (remove mod-id local :key #'first)))
            (:deactivate
             (setf (getf (rest entry) :active) NIL))
            (:error
             (cerror "Ignore the extra mod" "Mod is present locally."))
            ((NIL))))))
    (write-local-modlist client local)))

(defmethod sync-modlist-to-remote ((client simple-client) &key)
  (let* ((local (load-local-modlist client))
         (remote (load-remote-modlist client))
         (to-unsubscribe (set-difference remote local :key #'first))
         (to-subscribe (set-difference local remote :key #'first)))
    (dolist (entry to-unsubscribe)
      (games/mods/unsubscribe client (default-game-id client) (first entry)))
    (dolist (entry to-subscribe)
      (games/mods/subscribe client (default-game-id client) (first entry)))
    (write-local-modlist client local)))

(defmethod update-local-mods ((client simple-client) &key (if-exists :supersede)
                                                          (if-does-not-exist NIL))
  (let ((local (load-local-modlist client)))
    (dolist (entry local)
      (destructuring-bind (mod-id &key file-id version freeze &allow-other-keys) entry
        (unless freeze
          (let ((latest (first (games/mods/files client (default-game-id client) mod-id :sort `((date-added . :desc)) :end 1))))
            (if latest
                (ecase if-exists
                  (:supersede
                   (when (< (date-added latest) (file-write-date (mod-directory client mod-id)))
                     (extract-modfile latest client)
                     (setf (getf (rest entry) :file-id) (id latest))))
                  (:overwrite
                   (extract-modfile latest client)
                   (setf (getf (rest entry) :file-id) (id latest))))
                (ecase if-does-not-exist
                  (:delete
                   (delete-directory (mod-directory client mod-id))
                   (setf local (remove mod-id local :key #'first)))
                  (:deactivate
                   (setf (getf (rest entry) :active) NIL))
                  (:error
                   (cerror "Ignore the missing mod" 'modfile-not-found :endpoint "games/mods/files"))
                  ((NIL))))))))
    (write-local-modlist client local)))

(defmethod determine-mod-properties ((client simple-client) target)
  (list (parse-integer (car (last (pathname-directory target))))
        :version (let ((asd (first (directory (make-pathname :name :wild :type "asd" :defaults target))))
                       (*package* (find-package "ASDF-USER")))
                   (when asd
                     (with-open-file (stream asd :direction :input)
                       (loop for form = (read stream NIL '#1=#:eof)
                             until (eq form '#1#)
                             do (when (and (listp form) (eq (first form) 'asdf:defsystem))
                                  (return (getf form :version)))))))))


