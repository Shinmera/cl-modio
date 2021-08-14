#|
 This file is a part of cl-modio
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.modio)

(defvar *debug* NIL)
(defvar *base-url* "https://api.mod.io/v1/")

(define-condition modio-condition (condition)
  ())

(define-condition request-error (error modio-condition)
  ((endpoint :initarg :endpoint :initform (error "ENDPOINT required.") :reader endpoint)
   (arguments :initarg :arguments :initform () :reader arguments)
   (error-code :initarg :error-code :initform NIL :reader error-code)
   (message :initarg :message :initform NIL :reader message))
  (:report (lambda (c s) (format s "The request to ~a failed (~a~@[, error-code ~a~])~@[:~%  ~a~]"
                                 (endpoint c) (type-of c) (error-code c) (message c)))))

(define-condition bad-request (request-error)
  ())

(define-condition invalid-access-key (request-error)
  ())

(define-condition permission-denied (request-error)
  ())

(define-condition account-deleted (permission-denied)
  ())

(define-condition account-banned (permission-denied)
  ())

(define-condition resource-not-found (request-error)
  ())

(define-condition game-not-found (resource-not-found)
  ())

(define-condition mod-not-found (resource-not-found)
  ())

(define-condition mod-deleted (resource-not-found)
  ())

(define-condition modfile-not-found (resource-not-found)
  ())

(define-condition comment-not-found (resource-not-found)
  ())

(define-condition user-not-found (resource-not-found)
  ())

(define-condition resource-already-exists (request-error)
  ())

(define-condition too-many-requests (request-error)
  ())

(define-condition server-error (request-error)
  ())

(define-condition service-unavailable (request-error)
  ())

(defun direct-request (endpoint &key (method :get) parameters headers)
  (multiple-value-bind (stream status headers)
      (let ((drakma:*header-stream* (if *debug* *error-output*)))
        (drakma:http-request
         (format NIL "~a~a" *base-url* endpoint)
         :method method
         :want-stream T
         :external-format-in :utf-8
         :external-format-out :utf-8
         :parameters parameters
         :additional-headers headers))
    (flet ((handle-error (type)
             (let ((data (ignore-errors (yason:parse stream))))
               (error type :endpoint endpoint
                           :arguments parameters
                           :error-code (if data (gethash "error_ref" data) status)
                           :message (when data (gethash "message" data))))))
      (case status
        ((200 201 204)
         (values (if (= 204 status) T (yason:parse stream))
                 (cdr (assoc :location headers))
                 (cdr (assoc :x-ratelimit-retryafter headers))))
        (400 (handle-error 'bad-request))
        (401 (handle-error 'invalid-access-key))
        ((402 403) (handle-error 'permission-denied))
        (404 (handle-error 'resource-not-found))
        (409 (handle-error 'resource-already-exists))
        (429 (handle-error 'too-many-requests))
        (500 (handle-error 'server-error))
        (503 (handle-error 'service-unavailable))
        (T (handle-error 'request-error))))))

(defclass client ()
  ((api-key :initarg :api-key :initform NIL :accessor api-key)
   (access-token :initarg :access-token :initform NIL :accessor access-token)
   (language :initarg :language :initform "English" :accessor language)
   (platform :initarg :platform :initform (detect-platform) :accessor platform)
   (portal :initarg :portal :initform (detect-portal) :accessor portal)
   (wait-until :initarg :wait-until :initform NIL :accessor wait-until)
   (valid-until :initarg :valid-until :initform NIL :accessor valid-until)
   (on-rate-limit :initarg :on-rate-limit :initform :sleep :accessor on-rate-limit)))

(defmethod print-object ((client client) stream)
  (print-unreadable-object (client stream :type T)
    (format stream "~:[UNAUTHENTICATED~;AUTHENTICATED~]~@[ UNTIL ~a~]~@[ RATE LIMITED FOR ~ds~]"
            (api-key client)
            (when (valid-until client) (format-time (valid-until client)))
            (when (wait-until client) (- (wait-until client) (get-universal-time))))))

(defmethod make-load-form ((client client) &optional env)
  (declare (ignore env))
  `(make-instance ',(type-of client)
                  :api-key ,(api-key client)
                  :valid-until ,(valid-until client)
                  :access-token ,(access-token client)
                  :language ,(language client)))

(defmethod extract-user-properties ((client client))
  (list (access-token client) (valid-until client) (language client)))

(defmethod restore-user-properties ((client client) properties)
  (destructuring-bind (access-token valid-until language) properties
    (setf (access-token client) access-token)
    (setf (valid-until client) valid-until)
    (setf (language client) language)
    client))

(defun process-parameter-value (val)
  (etypecase val
    (string val)
    (pathname val)
    ((eql NIL) "false")
    ((eql T) "true")
    ((eql :false) "false")
    ((eql :true) "true")
    (symbol (string-downcase val))
    (ratio (princ-to-string (float val)))
    (real (princ-to-string val))
    (list (format NIL "~{~a~^,~}" val))))

(defun process-headers (parameters)
  (loop for (key val) on parameters by #'cddr
        when val
        collect (cons (string key) (process-parameter-value val))))

(defun process-parameters (parameters)
  (loop for (key val) on parameters by #'cddr
        when val
        collect (cons (to-parameter-name key) (process-parameter-value val))))

(defun process-filter (filter)
  (flet ((inner (filter)
           (ecase (first filter)
             (equal
              (list* NIL (third filter) (second filter)))
             (find
              (list "in" (third filter) (format NIL "~{~a~^,~}" (second filter))))
             (search
              (list* "_q" (third filter) (second filter)))
             (equalp
              (list* "lk" (third filter) (second filter))))))
    (case (first filter)
      (max
       (list* "max" (third filter) (second filter)))
      (min
       (list* "min" (third filter) (second filter)))
      (logand
       (list* "bitwise-and" (third filter) (second filter)))
      (not
       (destructuring-bind (match key val) (inner (second filter))
         (list (format NIL "~a-not~@[-~a~]" (when key (to-parameter-name key)) match) val)))
      (T
       (destructuring-bind (match key val) (inner filter)
         (list (format NIL "~a~@[-~a~]" (when key (to-parameter-name key)) match) val))))))

(defun process-sort (sort)
  (if (listp sort)
      (destructuring-bind (sort order) sort
        (list "_sort" (ecase order
                        (:asc (process-parameter-value sort))
                        (:desc (format NIL "-~a" (process-parameter-value sort))))))
      (list "_sort" (prcoess-parameter-value sort))))

(defmethod request ((client client) endpoint &key on-rate-limit parameters (method :get))
  (when (wait-until client)
    (etypecase (or on-rate-limit (on-rate-limit client))
      ((eql :return)
       NIL)
      ((eql :sleep)
       (sleep (- (wait-until client) (get-universal-time))))
      ((eql :error)
       (error 'too-many-requests :request (list* endpoint parameters)))
      ((or symbol function)
       (funcall on-rate-limit))))
  (multiple-value-bind (data location retry-after)
      (direct-request endpoint
                      :method method
                      :headers (process-headers
                                (list :accept "application/json"
                                      :authorization (access-token client)
                                      :accept-language (or (first (language-codes:codes (language client)))
                                                           (language client))
                                      :x-modio-platform (platform client)
                                      :x-modio-portal (portal client)))
                      :parameters (process-parameters
                                   (list* :api-key (api-key client)
                                          parameters)))
    (when retry-after
      (setf (wait-until client) (+ (get-universal-time) retry-after)))
    (values data location)))

(defmethod request-list ((client client) endpoint &key collect-results key on-rate-limit parameters start end per-page sort filter)
  (let ((parameters (list* "_limit" (or per-page 100)
                           (append parameters
                                   (when sort (process-sort sort))
                                   (when filter (mapcan #'process-filter filter)))))
        (start (or start 0))
        (end (or end most-positive-fixnum))
        (key (etypecase key
               (null #'identity)
               (symbol (fdefinition key))
               (function key)))
        (results ()))
    (let ((on-results (if collect-results
                          (lambda (r) (push (funcall key r) results))
                          (null key))))
      (restart-case
          (loop while (< start end)
                for data = (request client endpoint :on-rate-limit on-rate-limit :parameters (list* "_offset" start parameters))
                do (setf end (min end (gethash "result_total" data)))
                   (setf start (+ start (gethash "result_count" data)))
                   (dolist (result (gethash "data" data))
                     (funcall on-results result))
                finally (return (nreverse results)))
        (new-value (value)
          value)))))

(defmacro define-endpoint (name args &body body)
  (unless (find '&key args)
    (setf args (append args '(&key))))
  (destructuring-bind (name &optional (endpoint name) (method :get)) (if (listp name) name (list name))
    `(defun ,name (client ,@args on-rate-limit)
       (flet ((request (&rest parameters)
                (request client ,(if (listp endpoint) endpoint (string-downcase endpoint))
                         :on-rate-limit on-rate-limit
                         :method ,method
                         :parameters parameters)))
         ,@body))))

(defmacro define-list-endpoint (name args &body body)
  (unless (find '&key args)
    (setf args (append args '(&key))))
  (destructuring-bind (name &optional (endpoint name)) (if (listp name) name (list name))
    `(defun ,name (client ,@args (collect-results T) key on-rate-limit start end per-page sort)
       (flet ((request (inner-key &rest keyargs)
                (request-list client ,(if (listp endpoint) endpoint (string-downcase endpoint))
                              :on-rate-limit on-rate-limit
                              :collect-results collect-results
                              :start start
                              :end end
                              :per-page per-page
                              :key (if key
                                       (lambda (v)
                                         (funcall key (funcall inner-key v)))
                                       inner-key)
                              :sort sort
                              :filter (filter-from-keywords keyargs))))
         (macrolet ((@ (updater value)
                        `(update-value ,value #',updater)))
           ,@body)))))

(defmacro define-list-endpoint* ((name result-type &optional endpoint &rest rargs) &body args)
  `(define-list-endpoint (,name ,(if endpoint
                                     `(format NIL ,endpoint ,@(loop for arg in rargs collect `(id ,arg)))
                                     name))
       (,@rargs &key ,@(mapcar #'unlist args))
     (request (transformer ',result-type)
              ,@(loop for (name . kargs) in (mapcar #'enlist args)
                      collect (or (getf kargs :parameter) (intern (string name) "KEYWORD"))
                      collect (destructuring-bind (&key key update bitfield &allow-other-keys) kargs
                                (let ((form name))
                                  (when key (setf form `(,key ,form)))
                                  (when bitfield (setf form `(f (logand (,bitfield ,form)))))
                                  (when update (setf form `(@ ,update ,form)))
                                  form))))))

(defmacro define-edit-endpoint (method (name result-type endpoint &rest rargs) &body args)
  `(define-endpoint (,name (format NIL ,endpoint ,@(loop for arg in rargs collect `(id ,arg))) ,method)
       (,@rargs
        ,@(loop for arg in args
                when (and (listp arg) (getf (rest arg) :required))
                collect (unlist arg))
        &key ,@(loop for arg in args
                     unless (and (listp arg) (getf (rest arg) :required))
                     collect (unlist arg)))
     (macrolet ((@ (updater value)
                    `(update-value ,value #',updater)))
       (let ((data (request ,@(loop for (name . kargs) in (mapcar #'enlist args)
                                    collect (or (getf kargs :parameter) (intern (string name) "KEYWORD"))
                                    collect (destructuring-bind (&key key update &allow-other-keys) kargs
                                              (let ((form name))
                                                (when key (setf form `(,key ,form)))
                                                (when update (setf form `(@ ,update ,form)))
                                                form))))))
         ,(if result-type
              `(fill-object-from-data ',result-type data)
              'data)))))
