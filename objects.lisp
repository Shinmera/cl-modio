#|
 This file is a part of cl-modio
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.modio)

(defclass unique-resource ()
  ((id :accessor id)))

(define-print-method unique-resource "@~a" id)

(defclass named-resource ()
  ((name :accessor name)
   (name-id :accessor name-id)))

(define-print-method named-resource "~a" (or (name-id named-resource) (name named-resource)))

(define-parsable-class image ()
  (filename
   original
   (thumnbails :parameter NIL :tabkey (50 "thumb_50x50"
                                          64 "thumb_64x64"
                                          100 "thumb_100x100"
                                          128 "thumb_128x128"
                                          256 "thumb_256x256"
                                          320 "thumb_320x180"
                                          640 "thumb_640x360"
                                          1280 "thumb_1280x720"))))

(define-parsable-class comment (unique-resource)
  (id
   mod-id
   (user :nest user)
   (date-added :key universal-timestamp)
   reply-id
   thread-position
   karma
   content))

(defmethod mod ((thing comment))
  (mods/get *client* (default-game-id *client*) (mod-id thing)))

(defmethod reply ((thing comment))
  (when (reply-id thing)
    (games/mods/comments/get *client* (default-game-id *client*) (mod-id thing) (reply-id thing))))

(define-parsable-class download ()
  (binary-url
   (date-expires :key universal-timestamp)))

(define-parsable-class game (named-resource unique-resource)
  (id
   (status :key id-status)
   (submitted-by :nest user)
   (date-added :key universal-timestamp)
   (date-updated :key universal-timestamp)
   (date-live :key universal-timestamp)
   (presentation :parameter "presentation_option" :key id-presentation-option)
   (submission :parameter "submission_option" :key id-submission-option)
   (curation :parameter "curation_option" :key id-curation-option)
   (community :parameter "community_options" :key id-community-options)
   (revenue :parameter "revenue_options" :key id-revenue-options)
   (api-access :parameter "api_access_options" :key id-api-access-options)
   (maturity :parameter "maturity_options" :key id-maturity-options)
   ugc-name
   (icon :nest image)
   (logo :nest image)
   (header :nest image)
   name
   name-id
   summary
   instructions
   instructions-url
   profile-url
   (stats :nest game-stats)
   (tag-options :nest game-tag-option)))

(define-parsable-class game-stats ()
  (game-id
   (mods :parameter "mods_count_total")
   (downloads :parameter NIL :tabkey (:today "mods_downloads_today"
                                      :total "mods_downloads_total"
                                      :daily "mods_downloads_daily_average"))
   (subscribers :parameter "mods_subscribers_total")
   (date-expires :key universal-timestamp)))

(defmethod game ((thing game-stats))
  (games/get *client* (game-id game-stats)))

(define-parsable-class game-tag-option (named-resource)
  (name
   (tag-type :parameter "type" :key id-game-tag-type)
   tags
   tag-count
   hidden
   locked))

(defmethod fill-object-from-data ((message (eql 'message)) data)
  (gethash "message" data))

(define-parsable-class mod-dependency (named-resource)
  (mod-id
   name
   (date-added :key universal-timestamp)))

(defmethod mod ((thing mod-dependency))
  (mods/get *client* (default-game-id *client*) (mod-id thing)))

(define-parsable-class mod-event (unique-resource)
  (id
   mod-id
   user-id
   (date-added :key universal-timestamp)
   (event-type :key id-event-type)))

(defmethod mod ((thing mod-event))
  (mods/get *client* (default-game-id *client*) (mod-id thing)))

(define-parsable-class mod-media ()
  ((youtube-urls :parameter "youtube")
   (sketchfab-urls :parameter "sketchfab")
   (images :nest image)))

(define-parsable-class mod (named-resource unique-resource)
  (id
   game-id
   (status :key id-status)
   (visibility :key id-visibility)
   (submitted-by :nest user)
   (date-added :key universal-timestamp)
   (date-updated :key universal-timestamp)
   (date-live :key universal-timestamp)
   (maturity :parameter "maturity_options" :key id-maturity-options)
   (logo :nest image)
   homepage-url
   name
   name-id
   summary
   (description :parameter "description_plaintext")
   (description-html :parameter "description")
   metadata-blob
   profile-url
   (media :nest mod-media)
   (stats :nest mod-stats)
   (metadata :parameter "metadata_kvp" :key extract-metadata)
   (tags :nest mod-tag)))

(defmethod game ((mod mod))
  (games/get *client* (game-id mod)))

(define-parsable-class mod-stats ()
  (mod-id
   (date-expires :key universal-timestamp)
   (popularity :parameter NIL :tabkey (:position "popularity_rank_position"
                                       :total "popularity_rank_total_mods"))
   (downloads :parameter NIL :tabkey (:today "downloads_today"
                                      :total "downloads_total"))
   (subscribers :parameter "subscribers_total")
   (rating :parameter NIL :tabkey (:total "ratings_total"
                                   :positive "ratings_positive"
                                   :negative "ratings_negative"
                                   :percentage "ratings_percentage_positive"
                                   :aggregate "ratings_weighted_aggregate"
                                   :text "ratings_display_text"))))

(defmethod mod ((thing mod-stats))
  (mods/get *client* (default-game-id *client*) (mod-id thing)))

(define-parsable-class mod-tag (named-resource)
  (name
   (date-added :key universal-timestamp)))

(define-parsable-class modfile (unique-resource)
  (id
   mod-id
   (date-added :key universal-timestamp)
   (date-scanned :key universal-timestamp)
   (virus-status :key id-virus-status)
   (virus-positive :key id-virus-positive)
   virus-hash
   file-size
   (file-hash :key (lambda (k) (gethash "md5" k)))
   file-name
   version
   changelog
   metadata-blob
   (download :nest download)))

(defmethod mod ((thing modfile))
  (mods/get *client* (default-game-id *client*) (mod-id thing)))

(define-parsable-class rating ()
  (game-id
   mod-id
   (rating :key id-rating)
   (date-added :key universal-timestamp)))

(defmethod game ((thing rating))
  (games/get *client* (game-id thing)))

(defmethod mod ((thing rating))
  (mods/get *client* (default-game-id *client*) (mod-id thing)))

(define-parsable-class team-member ()
  (id
   (user :nest user)
   (level :key id-level)
   (date-added :key universal-timestamp)
   (title :parameter "position")
   (invite :parameter "invite_pending" :key id-invite)))

(define-parsable-class user-event (unique-resource)
  (id
   game-id
   mod-id
   user-id
   (date-added :key universal-timestamp)
   (event-type :key id-event-type)))

(defmethod game ((thing user-event))
  (games/get *client* (game-id thing)))

(defmethod mod ((thing user-event))
  (mods/get *client* (default-game-id *client*) (mod-id thing)))

(define-parsable-class user (named-resource unique-resource)
  (id
   (name :parameter "username")
   name-id
   (display-name :parameter "display_name_portal")
   (last-online :parameter "date_online" :key universal-timestamp)
   (avatar :nest image)
   profile-url))
