#|
 This file is a part of cl-modio
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.modio)

(defun complete-authentication (client data)
  (setf (access-token client) (gethash "access_token"  data))
  client)

(define-endpoint authenticate/terms (service)
  (let ((data (request :service service)))
    (values (gethash "plaintext" data) data)))

(define-endpoint (authenticate/email-request "oauth/emailrequest") (email)
  (request :email email))

(define-endpoint (authenticate/email-exchange "oauth/emailexchange") (security-code &key expires)
  (complete-authentication
   client
   (request :security-code security-code
            :date-expires (unix-timestamp expires))))

(define-endpoint (authenticate/steam "external/steamauth") (app-data &key email terms-agreed expires)
  (complete-authentication
   client
   (request :appdata app-data
            :email email
            :date-expires (unix-timestamp expires)
            :terms-agreed terms-agreed)))

(define-endpoint (authenticate/gog-galaxy "external/galaxyauth") (app-data &key email terms-agreed expires)
  (complete-authentication
   client
   (request :appdata app-data
            :email email
            :date-expires (unix-timestamp expires)
            :terms-agreed terms-agreed)))

(define-endpoint (authenticate/itchio "external/itchioauth") (token &key email terms-agreed expires)
  (complete-authentication
   client
   (request :itchio-token token
            :email email
            :date-expires (unix-timestamp expires)
            :terms-agreed terms-agreed)))

(define-endpoint (authenticate/oculus "external/oculusauth") (device nonce user-id access-token &key email terms-agreed expires)
  (complete-authentication
   client
   (request :device device
            :email email
            :nonce nonce
            :user-id user-id
            :access-token access-token
            :date-expires (unix-timestamp expires)
            :terms-agreed terms-agreed)))

(define-endpoint (authenticate/xbox "external/xboxauth") (token &key email terms-agreed expires)
  (complete-authentication
   client
   (request :xbox-token token
            :email email
            :date-expires (unix-timestamp expires)
            :terms-agreed terms-agreed)))

(define-endpoint (authenticate/switch "external/switchauth") (token &key email terms-agreed expires)
  (complete-authentication
   client
   (request :id-token token
            :email email
            :date-expires (unix-timestamp expires)
            :terms-agreed terms-agreed)))

(define-endpoint (authenticate/google "external/googleauth") (token &key email terms-agreed expires)
  (complete-authentication
   client
   (request :id-token token
            :email email
            :date-expires (unix-timestamp expires)
            :terms-agreed terms-agreed)))

(define-endpoint (authenticate/discord "external/discordauth") (token &key email terms-agreed expires)
  (complete-authentication
   client
   (request :discord-token token
            :email email
            :date-expires (unix-timestamp expires)
            :terms-agreed terms-agreed)))
