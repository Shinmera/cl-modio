#|
 This file is a part of cl-modio
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.modio)

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

(defun direct-request (endpoint &rest args)
  (multiple-value-bind (stream status headers) (apply #'drakma:http-request
                                                      (format NIL "~a~a" *base-url* endpoint)
                                                      :want-stream T
                                                      :external-format-in :utf-8
                                                      :external-format-out :utf-8
                                                      :want-stream T
                                                      args)
    (flet ((handle-error (type)
             (let ((data (ignore-errors (yason:parse stream))))
               (error type :endpoint endpoint
                           :arguments args
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
   (platform :initarg :platform :initform NIL :accessor platform)
   (portal :initarg :portal :initform NIL :accessor portal)
   (wait-until :initarg :wait-until :initform NIL :accessor wait-until)
   (on-rate-limit :initarg :on-rate-limit :initform :sleep :accessor on-rate-limit)))

(defun process-parameter-value (val)
  (etypecase val
    (string val)
    ((eql NIL) "false")
    ((eql T) "true")
    (symbol (string-downcase val))
    (ratio (princ-to-string (float val)))
    (real (princ-to-string val))))

(defun process-parameter-key (key)
  (flet ((key-char (char)
           (case char
             (#\- #\_)
             (T (char-downcase char)))))
    (map 'string #'key-char (string key))))

(defun process-headers (parameters)
  (loop for (key val) on parameters by #'cddr
        when val
        collect (cons (string key) (process-parameter-value val))))

(defun process-parameters (parameters)
  (loop for (key val) on parameters by #'cddr
        when val
        collect (cons (process-parameter-key val) (process-parameter-value val))))

(defun process-filter (filter)
  (flet ((inner (filter)
           (ecase (first filter)
             (=
              (list* NIL (rest filter)))
             (find
              (list "in" (second filter) (format NIL "~{~a~^,~}" (third filter))))
             (search
              (list* "_q" (rest filter)))
             (equal
              (list* "lk" (rest fliter))))))
    (case (first filter)
      (max
       (list* "max" (rest filter)))
      (min
       (list* "min" (rest filter)))
      (logand
       (list* "bitwise-and" (rest filter)))
      (not
       (destructuring-bind (match key val) (inner (second filter))
         (list (format NIL "~a-not~@[-~a~]" (when key (process-parameter-key key)) match) val)))
      (T
       (destructuring-bind (match key val) (inner filter)
         (list (format NIL "~a~@[-~a~]" (when key (process-parameter-key key)) match) val))))))

(defun process-sort (sort)
  (if (listp sort)
      (destructuring-bind (sort order) sort
        (list "_sort" (ecase order
                        (:asc (process-parameter-value sort))
                        (:desc (format NIL "-~a" (process-parameter-value sort))))))
      (list "_sort" (prcoess-parameter-value sort))))

(defmethod request ((client client) endpoint &key on-rate-limit parameters)
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
                      :additional-headers (process-headers
                                           :accept "application/json"
                                           :authorization (access-token client)
                                           :accept-language (or (first (language-codes:codes (language client)))
                                                                (language client))
                                           :x-modio-platform (platform client)
                                           :x-modio-portal (portal client))
                      :parameters (apply #'process-parameters
                                         :api-key (api-key client)
                                         parameters))
    (when retry-after
      (setf (wait-until client) (+ (get-universal-time) retry-after)))
    (values data location)))

(defmethod request-list ((client client) endpoint &key (on-results :collect) on-rate-limit parameters start end (per-page 100) sort filter)
  (let ((parameters (list* "_limit" per-page
                           (append parameters
                                   (when sort (process-sort sort))
                                   (when filter (mapcan #'process-filter filter)))))
        (start (or start 0))
        (end (or end most-positive-fixnum))
        (results ()))
    (let ((on-results (etypecase on-results
                        (null (constantly T))
                        ((eql :collect) (lambda (r)
                                          (push r results)))
                        (symbol (fdefinition on-results))
                        (function on-results))))
      (restart-case
          (loop while (< start end)
                for data = (request client endpoint :on-rate-limit on-rate-limit :parameters (list* "_offset" start parameters))
                do (setf end (min end (gethash data "result_total")))
                   (setf start (+ start (gethash data "result_count")))
                   (dolist (result (gethash data "data"))
                     (funcall on-results result))
                finally (return (nerverse results)))
        (new-value (value)
          value)))))

(defmacro define-endpoint (name args &body body)
  (unless (find '&key args)
    (setf args (append args '(&key))))
  (let ((endpoint (string-downcase name)))
    `(defun ,name (client ,@args on-rate-limit)
       (flet ((request (&rest parameters)
                (request client ,endpoint :on-rate-limit on-rate-limit :parameters parameters)))
         ,@body))))
