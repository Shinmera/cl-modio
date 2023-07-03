(in-package #:org.shirakumo.fraf.modio)

(defclass unique-resource ()
  ((id :accessor id)))

(define-print-method unique-resource "@~a" id)

(defclass named-resource ()
  ((name :initform NIL :accessor name)
   (name-id :initform NIL :accessor name-id)))

(define-print-method named-resource "~a" (or (name-id named-resource) (name named-resource)))

(defclass mod-resource ()
  ((mod-id :accessor mod-id)))

(defmethod mod ((thing mod-resource))
  (games/mods/get *client* (default-game-id *client*) (mod-id thing)))

(defclass game-resource ()
  ((game-id :accessor game-id)))

(defmethod game ((thing game-resource))
  (games/get *client* (game-id thing)))

(defclass user-resource ()
  ((user-id :accessor user-id)))

(define-parsable-class image ()
  ((file-name :parameter "filename")
   original
   (thumbnails :parameter NIL :tabkey (50 "thumb_50x50"
                                          64 "thumb_64x64"
                                          100 "thumb_100x100"
                                          128 "thumb_128x128"
                                          256 "thumb_256x256"
                                          320 "thumb_320x180"
                                          640 "thumb_640x360"
                                          1280 "thumb_1280x720"))))

(define-parsable-class comment (unique-resource mod-resource)
  (id
   mod-id
   (user :nest user)
   (date-added :key universal-timestamp)
   reply-id
   thread-position
   karma
   content))

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

(defmethod user ((game game))
  (general/ownership *client* :game (id game)))

(define-parsable-class game-stats (game-resource)
  (game-id
   (mods :parameter "mods_count_total")
   (downloads :parameter NIL :tabkey (:today "mods_downloads_today"
                                      :total "mods_downloads_total"
                                      :daily "mods_downloads_daily_average"))
   (subscribers :parameter "mods_subscribers_total")
   (date-expires :key universal-timestamp)))

(define-parsable-class game-tag-option (named-resource)
  (name
   (tag-type :parameter "type" :key id-game-tag-type)
   tags
   tag-count
   hidden
   locked))

(defmethod fill-object-from-data ((message (eql 'message)) data)
  (gethash "message" data))

(define-parsable-class mod-dependency (named-resource mod-resource)
  (mod-id
   name
   (date-added :key universal-timestamp)))

(define-parsable-class mod-event (unique-resource mod-resource user-resource)
  (id
   mod-id
   user-id
   (date-added :key universal-timestamp)
   (event-type :key id-event-type)))

(define-parsable-class mod-media ()
  ((youtube-urls :parameter "youtube")
   (sketchfab-urls :parameter "sketchfab")
   (images :nest image)))

(define-parsable-class mod (named-resource unique-resource game-resource)
  (id
   game-id
   (status :key id-status)
   (visible :key id-visibility)
   (submitted-by :nest user)
   (date-added :key universal-timestamp)
   (date-updated :key universal-timestamp)
   (date-live :key universal-timestamp)
   (maturity :parameter "maturity_option" :key id-maturity-options)
   (logo :nest image)
   homepage-url
   name
   name-id
   summary
   (description :parameter "description_plaintext")
   (description-html :parameter "description")
   (modfile :nest modfile)
   metadata-blob
   profile-url
   (media :nest mod-media)
   (stats :nest mod-stats)
   (metadata :parameter "metadata_kvp" :key extract-metadata)
   (tags :nest mod-tag)))

(defmethod user ((mod mod))
  (general/ownership *client* :mod (id mod)))

(define-parsable-class mod-stats (mod-resource)
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

(define-parsable-class mod-tag (named-resource)
  (name
   (date-added :key universal-timestamp)))

(define-parsable-class modfile (unique-resource mod-resource)
  (id
   mod-id
   (date-added :key universal-timestamp)
   (date-scanned :key universal-timestamp)
   (virus-status :key id-virus-status)
   (virus-positive :key id-virus-positive)
   (virus-hash :parameter "virustotal_hash")
   (file-size :parameter "filesize")
   (file-hash :parameter "filehash" :key (lambda (k) (gethash "md5" k)))
   (file-name :parameter "filename")
   version
   changelog
   metadata-blob
   (download :nest download)))

(defmethod user ((modfile modfile))
  (general/ownership *client* :file (id modfile)))

(define-parsable-class rating (mod-resource game-resource)
  (game-id
   mod-id
   (rating :key id-rating)
   (date-added :key universal-timestamp)))

(define-parsable-class team-member (unique-resource)
  (id
   (user :nest user)
   (level :key id-level)
   (date-added :key universal-timestamp)
   (title :parameter "position")
   (invite :parameter "invite_pending" :key id-invite)))

(define-parsable-class user-event (unique-resource mod-resource game-resource user-resource)
  (id
   game-id
   mod-id
   user-id
   (date-added :key universal-timestamp)
   (event-type :key id-event-type)))

(define-parsable-class user (named-resource unique-resource)
  (id
   (name :parameter "username")
   name-id
   (display-name :parameter "display_name_portal")
   (last-online :parameter "date_online" :key universal-timestamp)
   (avatar :nest image)
   profile-url))
