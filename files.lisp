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

;; TODO: auto-zip if pathname is a folder, etc.
(define-edit-endpoint :post (games/mods/files/add modfile "games/~a/mods/~a/files" game mod)
  (filedata :required T)
  version
  changelog
  active
  (file-hash :parameter :filehash)
  metadata-blob)

(define-edit-endpoint :put (games/mods/files/edit modfile "games/~a/mods/~a/files/~a" game mod file)
  version
  changelog
  active
  metadata-blob)

(define-edit-endpoint :delete (games/mods/files/edit NIL "games/~a/mods/~a/files/~a" game mod file))
