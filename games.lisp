#|
 This file is a part of cl-modio
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.modio)

(define-list-endpoint* (games game)
  (game :parameter :id :update id)
  (status :update status-id)
  (submitted-by :update id)
  (date-added :update unix-timestamp)
  (date-updated :update unix-timestamp)
  (date-live :update unix-timestamp)
  name
  name-id
  summary
  instructions-url
  ugc-name
  (presentation :parameter "presentation_option" :update presentation-option-id)
  (submission :parameter "submission_option" :update submission-option-id)
  (curation :parameter "curation_option" :update curation-option-id)
  (community :parameter "community_options" :bitfield community-options-id)
  (revenue :parameter "revenue_options" :bitfield revenue-options-id)
  (api-access :parameter "api_access_options" :bitfield api-access-options-id)
  (maturity :parameter "maturity_options" :bitfield maturity-options-id))

(define-endpoint (games/get (format NIL "games/~a" (id id))) (id)
  (fill-object-from-data 'game (request)))

(define-edit-endpoint :put (games/edit game "games/~a" game)
  (status :update status-id)
  name
  name-id
  summary
  instructions
  instructions-url
  ugc-name
  (presentation :update presentation-option-id)
  (submission :update submission-option-id)
  (curation :update curation-option-id)
  (community :update community-options-id)
  (revenue :update revenue-options-id)
  (api-access :update api-access-options-id)
  (maturity :update maturity-options-id))
