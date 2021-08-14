#|
 This file is a part of cl-modio
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.modio)

(define-edit-endpoint :post (games/mods/subscribe mod "games/~a/mods/~a/subscribe" game mod))
(define-edit-endpoint :delete (games/mods/unsubscribe NIL "games/~a/mods/~a/subscribe" game mod))
