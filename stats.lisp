#|
 This file is a part of cl-modio
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.modio)

(define-list-endpoint* (games/mods/stats/all mod-stats "games/~a/mods/stats" game)
  (mod :parameter :mod-id :update id)
  popularity-rank-position
  popularity-rank-total-mods
  (total-downloads :parameter :downloads-total)
  (total-subscribers :parameter :subscribers-total)
  (positive-ratings :parameter :ratings-positive)
  (negative-ratings :parameter :ratings-negative))

(define-list-endpoint* (games/mods/stats mod-stats "games/~a/mods/~a/stats" game mod))

(define-list-endpoint* (games/stats game-stats "games/~a/stats" game))
