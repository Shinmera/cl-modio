(in-package #:org.shirakumo.fraf.modio)

(define-list-endpoint* (games/mods/comments comment "games/~a/mods/~a/comments" game mod)
  (comment :parameter :id :update id)
  (submitted-by :update id)
  (date-added :update unix-timestamp)
  (reply :parameter :reply-id :update id)
  thread-position
  karma
  content)

(define-edit-endpoint :get (games/mods/comments/get comment "games/~a/mods/~a/comments/~a" game mod comment))

(define-edit-endpoint :post (games/mods/comments/add comment "games/~a/mods/~a/comments/~a" game mod comment)
  (content :required T)
  (reply :parameter :reply-id :update id))

(define-edit-endpoint :put (games/mods/comments/edit comment "games/~a/mods/~a/comments/~a" game mod comment)
  (content :required T))

(define-edit-endpoint :delete (games/mods/comments/delete NIL "games/~a/mods/~a/comments/~a" game mod comment))
