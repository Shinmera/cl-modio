(in-package #:org.shirakumo.fraf.modio)

(define-list-endpoint* (games/mods/team team-member "games/~a/mods/~a/team" game mod)
  (member :parameter :id :update id)
  (user :parameter :user-id :update id)
  username
  (level :update level-id)
  (date-added :update unix-timestamp)
  (pending :update invite-id))

(define-edit-endpoint :post (games/mods/team/add message "games/~a/mods/~a/team" game mod)
  (email :required T)
  (level :required T :update level-id)
  (title :parameter :position))

(define-edit-endpoint :put (games/mods/team/edit message "games/~a/mods/~a/team/~a" game mod member)
  (level :update level-id)
  (title :parameter :position)
  leader)

(define-edit-endpoint :delete (games/mods/team/delete NIL "games/~a/mods/~a/team/~a" game mod member))
