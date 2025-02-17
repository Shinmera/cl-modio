(in-package #:org.shirakumo.fraf.modio)

(defvar *portal* NIL)

(defun detect-platform ()
  #+windows :windows
  #+linux :linux
  #+darwin :mac
  #+android :android
  #+ios :ios
  #+xboxone :xboxone
  #+xboxseriesx :xboxseriesx
  #+ps4 :ps4
  #+ps5 :ps5
  #+switch :switch
  #+wii :wii)

(defun detect-portal ()
  (cond (*portal*
         *portal*)
        ((and (find-package '#:org.shirakumo.fraf.steamworks)
              (funcall (find-symbol (string '#:steamworks-available-p) '#:org.shirakumo.fraf.steamworks)))
         :steam)))

(defun universal-timestamp (unix-time)
  (+ unix-time (encode-universal-time 0 0 0 1 1 1970 0)))

(defun unix-timestamp (&optional (time (get-universal-time)))
  (- time (encode-universal-time 0 0 0 1 1 1970 0)))

(defun expiry-timestamp (&optional (time (get-universal-time)))
  (+ time 31536000))

(defun format-time (time)
  (multiple-value-bind (s m h dd mm yy) (decode-universal-time time 0)
    (format NIL "~4,'0d.~2,'0d.~2,'0d ~2,'0d:~2,'0d:~2,'0d" yy mm dd h m s)))

(defmacro define-print-method (class format &rest args)
  `(defmethod print-object ((,class ,class) stream)
     (print-unreadable-object (,class stream :type T)
       (format stream ,format ,@(loop for arg in args
                                      collect (etypecase arg
                                                (symbol `(,arg ,class))
                                                (cons arg)))))))

(defun to-parameter-name (key)
  (flet ((key-char (char)
           (case char
             (#\- #\_)
             (T (char-downcase char)))))
    (map 'string #'key-char (string key))))

(defun getenv (x)
  #+(or abcl clasp clisp ecl xcl) (ext:getenv x)
  #+allegro (sys:getenv x)
  #+clozure (ccl:getenv x)
  #+cmucl (unix:unix-getenv x)
  #+lispworks (lispworks:environment-variable x)
  #+sbcl (sb-ext:posix-getenv x)
  #-(or abcl clasp clisp ecl xcl allegro clozure cmucl lispworks sbcl)
  NIL)

(defun tempdir ()
  (pathname
   (format NIL "~a/"
           #+windows
           (or (getenv "TEMP")
               "~/AppData/Local/Temp")
           #+darwin
           (or (getenv "TMPDIR")
               "/tmp")
           #+linux
           (or (getenv "XDG_RUNTIME_DIR")
               "/tmp")
           #-(or windows darwin linux)
           "/tmp")))

(defun make-temp-file (&key (name (format NIL "modio-~a-~3,'0d" (get-universal-time) (random 1000))) type)
  (make-pathname :name name
                 :type type
                 :defaults (tempdir)))

(defun delete-directory (directory)
  #+sbcl (sb-ext:delete-directory directory :recursive T)
  #-sbcl (labels ((r (dir)
                    (dolist (file (directory (merge-pathnames (make-pathname :name :wild :type :wild :directory '(:relative :wild))
                                                              dir)))
                      (if (or (pathname-type file) (pathname-name file))
                          (delete-file file)
                          (r file)))
                    (delete-file dir)))
           (r directory)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun unlist (listish)
    (if (listp listish)
        (first listish)
        listish))

  (defun enlist (listish &rest defaults)
    (if (listp listish)
        listish
        (list* listish defaults))))

(defgeneric fill-object-from-data (object data))

(defmethod fill-object-from-data ((name symbol) data)
  (fill-object-from-data (make-instance name) data))

(defmethod fill-object-from-data ((name symbol) (data cons))
  (loop for inner in data
        collect (fill-object-from-data name inner)))

(defmethod fill-object-from-data ((name symbol) (data vector))
  (loop for inner across data
        collect (fill-object-from-data name inner)))

(defmethod fill-object-from-data ((name symbol) (data null))
  ())

(defun or* (&rest args)
  (loop for arg in args
        when (and arg (not (eql 'null arg)))
        return arg))

(defmacro define-parsable-class (name superclasses slots &rest options)
  (labels ((compile-slot-definition (slot)
             (destructuring-bind (name &key (initarg (intern (string name) "KEYWORD"))
                                            (initform NIL)
                                            (reader name)
                                            &allow-other-keys)
                 (enlist slot)
               `(,name :initarg ,initarg :initform ,initform :reader ,reader)))
           (compile-slot-extractor (slot)
             (destructuring-bind (name &key (parameter (to-parameter-name name))
                                            key
                                            tabkey
                                            nest
                                            &allow-other-keys)
                 (enlist slot)
               (let* ((form (if parameter `(or* (gethash ,parameter data)) 'data)))
                 (when key (setf form `(,key ,form)))
                 (when tabkey (setf form `(funcall (tabkey ,@tabkey) ,form)))
                 (when nest (setf form `(fill-object-from-data ',nest ,form)))
                 form))))
    `(progn
       (defclass ,name ,superclasses
         ,(mapcar #'compile-slot-definition slots)
         ,@options)

       (defmethod fill-object-from-data ((object ,name) (data hash-table))
         (when (< 0 (hash-table-count data))
           ,@(loop for slot in slots
                   collect `(setf (slot-value object ',(unlist slot)) ,(compile-slot-extractor slot))))
         object))))

(defmethod id ((id integer)) id)

(defmacro define-id-map (name &body associations)
  `(progn
     (defun ,(intern (format NIL "~a-~a" name 'id)) (value)
       (ecase value
         ,@associations))
     
     (defun ,(intern (format NIL "~a-~a" 'id name)) (value)
       ,(if (find-if #'stringp associations :key #'second)
            `(cond ,@(loop for (keys id) in associations
                           collect (list `(string-equal value ,id) (unlist keys)))
                   (T (error "Bad value")))
            `(ecase value
               ,@(loop for (keys id) in associations
                       collect (list id (unlist keys))))))))

(defmacro define-id-group (name &body associations)
  `(progn
     (defun ,(intern (format NIL "~a-~a" name 'id)) (value)
       (reduce #'logior value
               :key (lambda (k)
                      (ecase k
                        ,@associations))))

     (defun ,(intern (format NIL "~a-~a" 'id name)) (value)
       (let ((result ()))
         ,@ (loop for (keys id) in associations
                  collect `(when (< 0 (logand ,id value))
                             (push ,(unlist keys) result)))
         result))))

(define-id-map status
  (:unaccepted 0)
  (:accepted 1)
  (:deleted 3))

(define-id-map visibility
  (:hidden 0)
  (:public 1))

(define-id-map game-tag-type
  (:checkboxes "checkboxes")
  (:dropdown "dropdowwn"))

(define-id-map virus-status
  (:unscanned 0)
  (:completed 1)
  (:in-prograss 2)
  (:too-large 3)
  (:file-not-found 4)
  (:scanning-error 5))

(define-id-map virus-positive
  (:no-threats 0)
  (:flagged 1))

(define-id-map rating
  ((:good :positive 1) +1)
  ((:none NIL 0) 0)
  ((:bad :negative -1) -1))

(define-id-map event-type
  (:join "USER_TEAM_JOIN")
  (:leave "USER_TEAM_LEAVE")
  (:subscribe "USER_SUBSCRIBE")
  (:unsubscribe "USER_UNSUBSCRIBE")
  (:file-changed "MODFILE_CHANGED")
  (:available "MOD_AVAILABLE")
  (:unavailable "MOD_UNAVAILABLE")
  (:edited "MOD_EDITED")
  (:deleted "MOD_DELETED")
  (:team-changed "MOD_TEAM_CHANGED"))

(define-id-map presentation-option
  (:grid 0)
  (:table 1))

(define-id-map submission-option
  (:api 0)
  (:anywhere 1))

(define-id-map curation-option
  (:none 0)
  (:paid 1)
  (:full 2))

(define-id-map invite
  (:accepted 0)
  (:pending 1))

(define-id-map resource-type
  (:game "games")
  (:mod "mods")
  (:file "files"))

(define-id-map report-type
  (:generic 0)
  (:dmca 1)
  (:not-working 2)
  (:rude-content 3)
  (:illegal-content 4)
  (:stolen-content 5)
  (:false-information 6)
  (:other 7))

(define-id-group level
  (:moderator 1)
  (:manager 4)
  (:administrator 8))

(define-id-group community-options
  (:comments 1)
  (:guides 2)
  (:disable-subscribe-text 4))

(define-id-group revenue-options
  (:selling-allowed 1)
  (:donations-allowed 2)
  (:trading-allowed 4)
  (:scarcity-control-allowed 8))

(define-id-group api-access-options
  (:3rd-parties-allowed 1)
  (:direct-download-allowed 2))

(define-id-group maturity-options
  (:allow 1))

(defun extract-metadata (values)
  (let ((table (make-hash-table :test 'equal)))
    (loop for value across values
          do (setf (gethash (gethash "metakey" value) table)
                   (gethash "metavalue" value)))
    table))

(defun tabkey (&rest parameters)
  (lambda (k)
    (loop for (key param) on parameters by #'cddr
          for val = (gethash param k '#1=#:none)
          unless (eq val '#1#) collect key
          unless (eq val '#1#) collect val)))

(defstruct (filter (:constructor %make-filter (comparator value &optional (invert NIL)))
                   (:copier NIL)
                   (:predicate NIL))
  (comparator T)
  (value T)
  (invert NIL))

(defun filter-from-keywords (kargs)
  (loop for (key val) on kargs by #'cddr
        when val
        collect (typecase val
                  (filter
                   (let ((filter `(,(filter-comparator val) ,(filter-value val) ,key)))
                     (if (filter-invert val)
                         `(not ,filter)
                         filter)))
                  (list
                   `(find ,val ,key))
                  (T
                   `(equal ,val ,key)))))

(defmacro f (expr)
  (flet ((wrap-inner (comp val)
           (ecase comp
             ((equal find search equalp)
              `(%make-filter ',comp ,val)))))
    (case (first expr)
      (not
       (let ((filter (apply #'wrap-inner (second expr))))
         `(,@filter T)))
      ((min max logand)
       `(%make-filter ',(first expr) ,(second expr)))
      (T
       (apply #'wrap-inner expr)))))

(defun update-value (value tx)
  (typecase value
    (filter
     (setf (filter-value value)
           (if (listp (filter-value value))
               (mapcar tx (filter-value value))
               (funcall tx (filter-value value))))
     value)
    (null
     value)
    (list
     (mapcar tx value))
    (T
     (funcall tx value))))
