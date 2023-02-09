#|
 This file is a part of cl-modio
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.modio)

(define-list-endpoint* (games/mods mod "games/~a/mods" game)
  (mod :parameter :id :update id)
  (status :update status-id)
  (visibility :parameter :visible :update visibility-id)
  (submitted-by :update id)
  (date-added :update unix-timestamp)
  (date-updated :update unix-timestamp)
  (date-live :update unix-timestamp)
  (maturity :parameter :maturity-option :bitfield maturity-options-id)
  name
  name-id
  summary
  description
  homepage-url
  (modfile :update id)
  metadata-blob
  metadata-kvp
  tags)

(define-edit-endpoint :get (games/mods/get mod "games/~a/mods/~a" game mod))

(define-edit-endpoint :post (games/mods/add mod "games/~a/mods" game)
  (name :required T)
  (summary :required T)
  (logo :required T)
  (visibility :parameter :visible :update visibility-id)
  name-id
  description
  homepage-url
  stock
  (maturity :parameter :maturity-option :bitfield maturity-options-id)
  metadata-blob
  tags)

(define-edit-endpoint :put (games/mods/edit mod "games/~a/mods/~a" game mod)
  logo
  (status :update status-id)
  (visibility :parameter :visible :update visibility-id)
  name
  name-id
  summary
  description
  homepage-url
  stock
  (maturity :parameter :maturity-option :bitfield maturity-options-id)
  metadata-blob)

(define-edit-endpoint :delete (games/mods/delete NIL "games/~a/mods/~a" game mod))
