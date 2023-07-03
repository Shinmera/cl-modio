(in-package #:org.shirakumo.fraf.modio)

(define-edit-endpoint :post (general/ownership user "/general/ownership")
  (type :required T :parameter :resource-type :update resource-type-id)
  (id :required T :parameter :resource-id :update id))
