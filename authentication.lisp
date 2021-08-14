#|
 This file is a part of cl-modio
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.modio)

(defun complete-authentication (client data)
  (setf (access-token client) (gethash "access_token" data))
  (setf (valid-until client) (universal-timestamp (gethash "date_expires" data)))
  client)

(define-endpoint authenticate/terms (service)
  (let ((data (request :service service)))
    (values (gethash "plaintext" data) data)))

(define-endpoint (authenticate/email-request "oauth/emailrequest" :post) (email)
  (fill-object-from-data 'message (request :email email)))

(define-endpoint (authenticate/email-exchange "oauth/emailexchange" :post) (security-code &key (expires (expiry-timestamp)))
  (complete-authentication
   client
   (request :security-code security-code
            :date-expires (unix-timestamp expires))))

(define-endpoint (authenticate/steam "external/steamauth" :post) (app-data &key email terms-agreed (expires (expiry-timestamp)))
  (complete-authentication
   client
   (request :appdata app-data
            :email email
            :date-expires (unix-timestamp expires)
            :terms-agreed terms-agreed)))

(define-endpoint (authenticate/gog-galaxy "external/galaxyauth" :post) (app-data &key email terms-agreed (expires (expiry-timestamp)))
  (complete-authentication
   client
   (request :appdata app-data
            :email email
            :date-expires (unix-timestamp expires)
            :terms-agreed terms-agreed)))

(define-endpoint (authenticate/itchio "external/itchioauth" :post) (token &key email terms-agreed (expires (expiry-timestamp)))
  (complete-authentication
   client
   (request :itchio-token token
            :email email
            :date-expires (unix-timestamp expires)
            :terms-agreed terms-agreed)))

(define-endpoint (authenticate/oculus "external/oculusauth" :post) (device nonce user-id access-token &key email terms-agreed (expires (expiry-timestamp)))
  (complete-authentication
   client
   (request :device device
            :email email
            :nonce nonce
            :user-id user-id
            :access-token access-token
            :date-expires (unix-timestamp expires)
            :terms-agreed terms-agreed)))

(define-endpoint (authenticate/xbox "external/xboxauth" :post) (token &key email terms-agreed (expires (expiry-timestamp)))
  (complete-authentication
   client
   (request :xbox-token token
            :email email
            :date-expires (unix-timestamp expires)
            :terms-agreed terms-agreed)))

(define-endpoint (authenticate/switch "external/switchauth" :post) (token &key email terms-agreed (expires (expiry-timestamp)))
  (complete-authentication
   client
   (request :id-token token
            :email email
            :date-expires (unix-timestamp expires)
            :terms-agreed terms-agreed)))

(define-endpoint (authenticate/google "external/googleauth" :post) (token &key email terms-agreed (expires (expiry-timestamp)))
  (complete-authentication
   client
   (request :id-token token
            :email email
            :date-expires (unix-timestamp expires)
            :terms-agreed terms-agreed)))

(define-endpoint (authenticate/discord "external/discordauth" :post) (token &key email terms-agreed (expires (expiry-timestamp)))
  (complete-authentication
   client
   (request :discord-token token
            :email email
            :date-expires (unix-timestamp expires)
            :terms-agreed terms-agreed)))
