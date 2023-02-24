#|
 This file is a part of cl-modio
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.modio)

(define-list-endpoint (games/mods/metadata (format nil "games/~a/mods/~a/metadatakvp" (id game) (id mod)))
    (game mod &key)
  (alexandria:alist-hash-table
   (request (lambda (k) (cons (gethash "metakey" k) (gethash "metavalue" k))))))

(define-edit-endpoint :post (games/mods/metadata/add message "games/~a/mods/~a/metadatakvp" game mod)
  (metadata[] :key (lambda (k)
                     (loop for key being the hash-keys of k
                           for val being the hash-values of k
                           collect (format NIL "~a:~a" key val)))
              :required T))

(define-edit-endpoint :delete (games/mods/metadata/delete NIL "games/~a/mods/~a/metadatakvp" game mod)
  (metadata[] :key (lambda (k)
                     (loop for key being the hash-keys of k
                           for val being the hash-values of k
                           collect (format NIL "~a~@[:~a~]" key val)))
              :required T))
