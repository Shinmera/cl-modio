#|
 This file is a part of cl-modio
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.modio)

(define-edit-endpoint :post (report message "/report")
  (type :required T :parameter :resource-type :update resource-type-id)
  (id :required T :update id)
  (report-type :required T :parameter :type :update report-type-id)
  name
  contact
  (summary :required T))
