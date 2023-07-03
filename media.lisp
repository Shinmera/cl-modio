(in-package #:org.shirakumo.fraf.modio)

(define-edit-endpoint :post (games/media/add message "games/~a/media" game)
  logo
  icon
  header)

(define-edit-endpoint :post (games/mods/media/add message "games/~a/mods/~a/media" game mod)
  logo
  images
  (youtube-urls :parameter :youtube)
  (sketchfab :parameter :sketchfab))

(define-edit-endpoint :delete (games/mods/media/delete NIL "games/~a/mods/~a/media" game mod))
