(in-package #:org.shirakumo.fraf.modio)

(define-list-endpoint* (games/mods/events event "games/~a/mods/events" game)
  (event :parameter :id :update id)
  (mod :parameter :mod-id :update id)
  (user :parameter :user-id :update id)
  (date-added :update unix-timestamp)
  (event-type :update event-type-id)
  latest subscribed)

(define-list-endpoint* (games/mods/mod-events event "games/~a/mods/~a/events" game mod))
