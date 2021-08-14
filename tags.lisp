#|
 This file is a part of cl-modio
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.modio)

(define-list-endpoint* (games/mods/tags mod-tag "games/~a/mods/~a/tags" game mod)
  (date-added :update unix-timestamp)
  tag)

(define-edit-endpoint :post (games/mods/tags/add message "games/~a/mods/~a/tags" game mod)
  tags)

(define-edit-endpoint :delete (games/mods/tags/delete NIL "games/~a/mods/~a/tags" game mod)
  tags)

(define-list-endpoint* (games/tags game-tag-option "games/~a/tags" game))

(define-edit-endpoint :post (games/tags/add message "games/~a/tags" game)
  (name :required T)
  (type :required T :update game-tag-type-id)
  hidden
  locked
  (tags :required T))

(define-edit-endpoint :delete (games/tags/delete NIL "games/~a/tags" game)
  (name :required T)
  (tags :required T))
