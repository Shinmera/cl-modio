#|
 This file is a part of cl-modio
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.modio
  (:use #:cl)
  (:shadow #:mod)
  ;; toolkit.lisp
  (:export
   #:*portal*
   #:detect-platform
   #:detect-portal)
  ;; client.lisp
  (:export
   #:*debug*
   #:*client*
   #:*base-url*
   #:modio-condition
   #:request-error
   #:endpoint
   #:arguments
   #:error-code
   #:message
   #:bad-request
   #:invalid-access-key
   #:permission-denied
   #:account-deleted
   #:account-banned
   #:resource-not-found
   #:game-not-found
   #:mod-not-found
   #:mod-deleted
   #:modfile-not-found
   #:comment-not-found
   #:user-not-found
   #:resource-already-exists
   #:too-many-requests
   #:server-error
   #:service-unavailable
   #:direct-request
   #:client
   #:api-key
   #:access-token
   #:language
   #:platform
   #:portal
   #:wait-until
   #:valid-until
   #:on-rate-limit
   #:cache
   #:default-game-id
   #:extract-user-properties
   #:restore-user-properties
   #:request
   #:request-list)
  ;; objects.lisp
  (:export
   #:unique-resource
   #:id
   #:named-resource
   #:name
   #:name-id
   #:image
   #:filename
   #:original
   #:thumbnails
   #:comment
   #:mod-id
   #:user
   #:date-added
   #:reply-id
   #:thread-position
   #:karma
   #:content
   #:mod
   #:reply
   #:download
   #:binary-url
   #:date-expires
   #:game
   #:status
   #:submitted-by
   #:date-added
   #:date-updated
   #:date-live
   #:presentation
   #:submission
   #:curation
   #:community
   #:revenue
   #:api-access
   #:maturity
   #:ugc-name
   #:icon
   #:logo
   #:header
   #:name
   #:name-id
   #:summary
   #:instructions
   #:instructions-url
   #:profile-url
   #:stats
   #:tag-options
   #:game-stats
   #:game-id
   #:mods
   #:downloads
   #:subscribers
   #:date-expires
   #:game
   #:game-tag-option
   #:name
   #:tag-type
   #:tags
   #:tag-count
   #:hidden
   #:locked
   #:mod-dependency
   #:mod-id
   #:name
   #:date-added
   #:mod-event
   #:mod-id
   #:user-id
   #:date-added
   #:event-type
   #:mod-media
   #:youtube-urls
   #:sketchfab-urls
   #:images
   #:mod
   #:game-id
   #:status
   #:visible
   #:submitted-by
   #:date-added
   #:date-updated
   #:date-live
   #:maturity
   #:logo
   #:homepage-url
   #:name
   #:name-id
   #:summary
   #:description
   #:description-html
   #:modfile
   #:metadata-blob
   #:profile-url
   #:media
   #:stats
   #:metadata
   #:tags
   #:mod-stats
   #:mod-id
   #:date-expires
   #:popularity
   #:downloads
   #:subscribers
   #:rating
   #:mod-tag
   #:name
   #:date-added
   #:modfile
   #:mod-id
   #:date-added
   #:date-scanned
   #:virus-status
   #:virus-positive
   #:virus-hash
   #:file-size
   #:file-hash
   #:file-name
   #:version
   #:changelog
   #:metadata-blob
   #:download
   #:rating
   #:game-id
   #:mod-id
   #:rating
   #:date-added
   #:team-member
   #:user
   #:level
   #:date-added
   #:title
   #:invite
   #:user-event
   #:game-id
   #:mod-id
   #:user-id
   #:date-added
   #:event-type
   #:user
   #:name
   #:name-id
   #:display-name
   #:last-online
   #:avatar
   #:profile-url)
  ;; cache.lisp
  (:export
   #:cache
   #:valid-p
   #:clear-cache)
  ;; authentication.lisp
  (:export
   #:authenticate/terms
   #:authenticate/logout
   #:authenticate/email-request
   #:authenticate/email-exchange
   #:authenticate/steam
   #:authenticate/epic
   #:authenticate/gog-galaxy
   #:authenticate/itchio
   #:authenticate/oculus
   #:authenticate/xbox
   #:authenticate/switch
   #:authenticate/google
   #:authenticate/discord
   #:authenticate/openid)
  ;; me.lisp
  (:export
   #:me
   #:me/subscribed
   #:me/events
   #:me/games
   #:me/mods
   #:me/files
   #:me/ratings)
  ;; games.lisp
  (:export
   #:games
   #:games/get
   #:games/edit)
  ;; mods.lisp
  (:export
   #:games/mods
   #:games/mods/get
   #:games/mods/add
   #:games/mods/edit
   #:games/mods/delete)
  ;; files.lisp
  (:export
   #:modfile-error
   #:modfile
   #:target
   #:download-corrupted
   #:file-size
   #:target-does-not-exist
   #:target-already-exists
   #:games/mods/files
   #:games/mods/files/get
   #:games/mods/files/add
   #:games/mods/files/edit
   #:games/mods/files/delete
   #:download-modfile
   #:extract-modfile)
  ;; subscribe.lisp
  (:export
   #:games/mods/subscribe
   #:games/mods/unsubscribe)
  ;; comments.lisp
  (:export
   #:games/mods/comments
   #:games/mods/comments/get
   #:games/mods/comments/add
   #:games/mods/comments/edit
   #:games/mods/comments/delete)
  ;; media.lisp
  (:export
   #:games/media/add
   #:games/mods/media/add
   #:games/mods/media/delete)
  ;; events.lisp
  (:export
   #:games/mods/events
   #:games/mods/mod-events)
  ;; tags.lisp
  (:export
   #:games/mods/tags
   #:games/mods/tags/add
   #:games/mods/tags/delete
   #:games/tags
   #:games/tags/add
   #:games/tags/delete)
  ;; ratings.lisp
  (:export
   #:games/mods/ratings/add)
  ;; stats.lisp
  (:export
   #:games/mods/stats/all
   #:games/mods/stats
   #:games/stats)
  ;; metadata.lisp
  (:export
   #:games/mods/metadata
   #:games/mods/metadata/add
   #:games/mods/metadata/delete)
  ;; dependencies.lisp
  (:export
   #:games/mods/dependencies
   #:games/mods/dependencies/add
   #:games/mods/dependencies/delete)
  ;; teams.lisp
  (:export
   #:games/mods/team
   #:games/mods/team/add
   #:games/mods/team/edit
   #:games/mods/team/delete)
  ;; general.lisp
  (:export
   #:general/ownership)
  ;; reports.lisp
  (:export
   #:report)
  ;; simple.lisp
  (:export
   #:simple-client
   #:mods-directory
   #:modlist-file
   #:modfile-cache-directory
   #:mod-directory
   #:load-local-modlist
   #:write-local-modlist
   #:load-remote-modlist
   #:download-modfile
   #:extract-modfile
   #:find-modfile
   #:sync-modlist-from-remote
   #:sync-modlist-to-remote
   #:update-local-mods
   #:determine-mod-properties))
