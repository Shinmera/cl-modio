#|
 This file is a part of cl-modio
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.modio)

(define-edit-endpoint :post (games/mods/ratings/add message "games/~a/mods/~a/ratings" game mod)
  (rating :required T :update rating-id))
