(in-package #:org.shirakumo.fraf.modio)

(define-list-endpoint* (games/mods/dependencies mod-dependency "games/~a/mods/~a/dependencies" game mod))

(define-edit-endpoint :post (games/mods/dependencies/add message "games/~a/mods/~a/dependencies" game mod)
  (dependencies :required T))

(define-edit-endpoint :delete (games/mods/dependencies/delete NIL "games/~a/mods/~a/dependencies" game mod)
  (dependencies :required T))

