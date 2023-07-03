(in-package #:org.shirakumo.fraf.modio)

(define-list-endpoint* (games/mods/stats/all mod-stats "games/~a/mods/stats" game)
  (mod :parameter :mod-id :update id)
  popularity-rank-position
  popularity-rank-total-mods
  (total-downloads :parameter :downloads-total)
  (total-subscribers :parameter :subscribers-total)
  (positive-ratings :parameter :ratings-positive)
  (negative-ratings :parameter :ratings-negative))

(define-edit-endpoint :get (games/mods/stats mod-stats "games/~a/mods/~a/stats" game mod))

(define-edit-endpoint :get (games/stats game-stats "games/~a/stats" game))
