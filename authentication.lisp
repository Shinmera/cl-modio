#|
 This file is a part of cl-modio
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.modio)

(defun complete-authentication (client data)
  (setf (access-token client) (gethash data "access_token"))
  client)

(define-endpoint authenticate/terms (service)
  (request :service service))

(define-endpoint oauth/emailrequest (email)
  (request :email email))

(define-endpoint oauth/emailexchange (security-code &key expires)
  (complete-authentication
   client
   (request :security-code security-code
            :date-expires (unix-timestamp expires))))

(define-endpoint external/steamauth (app-data &key email terms-agreed expires)
  (complete-authentication
   client
   (request :appdata app-data
            :email email
            :date-expires (unix-timestamp expires)
            :terms-agreed terms-agreed)))

(define-endpoint external/galaxyauth (app-data &key email terms-agreed expires)
  (complete-authentication
   client
   (request :appdata app-data
            :email email
            :date-expires (unix-timestamp expires)
            :terms-agreed terms-agreed)))

(define-endpoint external/itchioauth (token &key email terms-agreed expires)
  (complete-authentication
   client
   (request :itchio-token token
            :email email
            :date-expires (unix-timestamp expires)
            :terms-agreed terms-agreed)))

(define-endpoint external/oculusauth (device nonce user-id access-token &key email terms-agreed expires)
  (complete-authentication
   client
   (request :device device
            :email email
            :nonce nonce
            :user-id user-id
            :access-token access-token
            :date-expires (unix-timestamp expires)
            :terms-agreed terms-agreed)))

(define-endpoint external/xboxauth (token &key email terms-agreed expires)
  (complete-authentication
   client
   (request :xbox-token token
            :email email
            :date-expires (unix-timestamp expires)
            :terms-agreed terms-agreed)))

(define-endpoint external/switchauth (token &key email terms-agreed expires)
  (complete-authentication
   client
   (request :id-token token
            :email email
            :date-expires (unix-timestamp expires)
            :terms-agreed terms-agreed)))

(define-endpoint external/googleauth (token &key email terms-agreed expires)
  (complete-authentication
   client
   (request :id-token token
            :email email
            :date-expires (unix-timestamp expires)
            :terms-agreed terms-agreed)))

(define-endpoint external/discordauth (token &key email terms-agreed expires)
  (complete-authentication
   client
   (request :discord-token token
            :email email
            :date-expires (unix-timestamp expires)
            :terms-agreed terms-agreed)))