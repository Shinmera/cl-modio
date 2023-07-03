(in-package #:org.shirakumo.fraf.modio)

(define-edit-endpoint :post (games/mods/ratings/add message "games/~a/mods/~a/ratings" game mod)
  (rating :required T :update rating-id))
