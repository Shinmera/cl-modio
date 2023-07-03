(in-package #:org.shirakumo.fraf.modio)

(define-endpoint me ()
  (cache-object client 'user (request)))

(define-list-endpoint* (me/subscribed mod)
  (mod :parameter :id :update id)
  (game :parameter :game-id :update id)
  (submitted-by :update id)
  (date-added :update unix-timestamp)
  (date-updated :update unix-timestamp)
  (date-live :update unix-timestamp)
  name
  name-id
  summary
  description
  homepage-url
  metadata-blob
  tags)

(define-list-endpoint* (me/events event)
  (event :parameter :id :update id)
  (game :parameter :game-id :update id)
  (mod :parameter :mod-id :update id)
  (user :parameter :user-id :update id)
  (date-added :update unix-timestamp)
  (event-type :update event-type-id))

(define-list-endpoint* (me/games game)
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
  (api-access :parameter "api_access_options" :bitfield api-access-options-id))

(define-list-endpoint* (me/mods mod)
  (mod :parameter :id :update id)
  (game :parameter :game-id :update id)
  (status :update status-id)
  (visibility :parameter :visible :update visibility-id)
  (submitted-by :update id)
  (date-added :update unix-timestamp)
  (date-updated :update unix-timestamp)
  (date-live :update unix-timestamp)
  name
  name-id
  summary
  description
  homepage-url
  (modfile :update id)
  metadata-blob
  metadata-kvp
  tags)

(define-list-endpoint* (me/files modfile)
  (modfile :parameter :id :update id)
  (mod :parameter :mod-id :update id)
  (date-added :update unix-timestamp)
  (date-scanned :update unix-timestamp)
  (virus-status :update virus-status-id)
  (virus-positive :update virus-positive-id)
  (file-size :parameter :filesize)
  (file-hash :parameter :filehash)
  (file-name :parameter :filename)
  version
  changelog
  metadata-blob)

(define-list-endpoint* (me/ratings rating)
  (game :parameter :game-id :update id)
  (mod :parameter :mod-id :update id)
  (rating :update rating-id)
  (date-added :update unix-timestamp))
