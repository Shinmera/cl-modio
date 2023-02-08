#|
 This file is a part of cl-modio
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.modio)

;; toolkit.lisp
(docs:define-docs
  (variable *portal*
    "")
  
  (function detect-platform
    "")
  
  (function detect-portal
    ""))

;; client.lisp
(docs:define-docs
  (variable *debug*
    "")
  
  (variable *client*
    "")
  
  (variable *base-url*
    "")
  
  (type modio-condition
    "")
  
  (type request-error
    "")
  
  (function endpoint
    "")
  
  (function arguments
    "")
  
  (function error-code
    "")
  
  (function message
    "")
  
  (type bad-request
    "")
  
  (type invalid-access-key
    "")
  
  (type permission-denied
    "")
  
  (type account-deleted
    "")
  
  (type account-banned
    "")
  
  (type resource-not-found
    "")
  
  (type game-not-found
    "")
  
  (type mod-not-found
    "")
  
  (type mod-deleted
    "")
  
  (type modfile-not-found
    "")
  
  (type comment-not-found
    "")
  
  (type user-not-found
    "")
  
  (type resource-already-exists
    "")
  
  (type too-many-requests
    "")
  
  (type server-error
    "")
  
  (type service-unavailable
    "")
  
  (function direct-request
    "")
  
  (type client
    "")
  
  (function api-key
    "")
  
  (function access-token
    "")
  
  (function language
    "")
  
  (function platform
    "")
  
  (function portal
    "")
  
  (function wait-until
    "")
  
  (function valid-until
    "")
  
  (function on-rate-limit
    "")
  
  (function cache
    "")
  
  (function default-game-id
    "")
  
  (function extract-user-properties
    "")
  
  (function restore-user-properties
    "")
  
  (function request
    "")
  
  (function request-list
    ""))

;; objects.lisp
(docs:define-docs
  (type unique-resource
    "")
  
  (function id
    "")
  
  (type named-resource
    "")
  
  (function name
    "")
  
  (function name-id
    "")
  
  (type image
    "")
  
  (function filename
    "")
  
  (function original
    "")
  
  (function thumbnails
    "")
  
  (type comment
    "")
  
  (function mod-id
    "")
  
  (function user
    "")
  
  (function date-added
    "")
  
  (function reply-id
    "")
  
  (function thread-position
    "")
  
  (function karma
    "")
  
  (function content
    "")
  
  (function mod
    "")
  
  (function reply
    "")
  
  (type download
    "")
  
  (function binary-url
    "")
  
  (function date-expires
    "")
  
  (type game
    "")
  
  (function status
    "")
  
  (function submitted-by
    "")
  
  (function date-updated
    "")
  
  (function date-live
    "")
  
  (function presentation
    "")
  
  (function submission
    "")
  
  (function curation
    "")
  
  (function community
    "")
  
  (function revenue
    "")
  
  (function api-access
    "")
  
  (function maturity
    "")
  
  (function ugc-name
    "")
  
  (function icon
    "")
  
  (function logo
    "")
  
  (function header
    "")
  
  (function summary
    "")
  
  (function instructions
    "")
  
  (function instructions-url
    "")
  
  (function profile-url
    "")
  
  (function stats
    "")
  
  (function tag-options
    "")
  
  (type game-stats
    "")
  
  (function game-id
    "")
  
  (function mods
    "")
  
  (function downloads
    "")
  
  (function subscribers
    "")
  
  (type game-tag-option
    "")
  
  (function tag-type
    "")
  
  (function tags
    "")
  
  (function tag-count
    "")
  
  (function hidden
    "")
  
  (function locked
    "")
  
  (type mod-dependency
    "")
  
  (type mod-event
    "")
  
  (function user-id
    "")
  
  (function event-type
    "")
  
  (type mod-media
    "")
  
  (function youtube-urls
    "")
  
  (function sketchfab-urls
    "")
  
  (function images
    "")
  
  (type mod
    "")
  
  (function visible
    "")
  
  (function homepage-url
    "")
  
  (function description
    "")
  
  (function description-html
    "")
  
  (function modfile
    "")
  
  (function metadata-blob
    "")
  
  (function media
    "")
  
  (function metadata
    "")
  
  (type mod-stats
    "")
  
  (function popularity
    "")
  
  (function rating
    "")
  
  (type mod-tag
    "")
  
  (function date-scanned
    "")
  
  (function virus-status
    "")
  
  (function virus-positive
    "")
  
  (function virus-hash
    "")
  
  (function file-size
    "")
  
  (function file-hash
    "")
  
  (function file-name
    "")
  
  (function version
    "")
  
  (function changelog
    "")
  
  (type rating
    "")
  
  (type team-member
    "")
  
  (function level
    "")
  
  (function title
    "")
  
  (function invite
    "")
  
  (type user-event
    "")
  
  (type user
    "")
  
  (function display-name
    "")
  
  (function last-online
    "")
  
  (function avatar
    "")
  
  (function profile-url
    ""))

;; cache.lisp
(docs:define-docs
  (function valid-p
    "")
  
  (function clear-cache
    ""))

;; authentication.lisp
(docs:define-docs
  (function authenticate/terms
    "")
  
  (function authenticate/logout
    "")
  
  (function authenticate/email-request
    "")
  
  (function authenticate/email-exchange
    "")
  
  (function authenticate/steam
    "")
  
  (function authenticate/epic
    "")
  
  (function authenticate/gog-galaxy
    "")
  
  (function authenticate/itchio
    "")
  
  (function authenticate/oculus
    "")
  
  (function authenticate/xbox
    "")
  
  (function authenticate/switch
    "")
  
  (function authenticate/google
    "")
  
  (function authenticate/discord
    "")
  
  (function authenticate/openid
    ""))

;; me.lisp
(docs:define-docs
  (function me
    "")
  
  (function me/subscribed
    "")
  
  (function me/events
    "")
  
  (function me/games
    "")
  
  (function me/mods
    "")
  
  (function me/files
    "")
  
  (function me/ratings
    ""))

;; games.lisp
(docs:define-docs
  (function games
    "")
  
  (function games/get
    "")
  
  (function games/edit
    ""))

;; mods.lisp
(docs:define-docs
  (function games/mods
    "")
  
  (function games/mods/get
    "")
  
  (function games/mods/add
    "")
  
  (function games/mods/edit
    "")
  
  (function games/mods/delete
    ""))

;; files.lisp
(docs:define-docs
  (function modfile-error
    "")
  
  (function target
    "")
  
  (function download-corrupted
    "")
  
  (function target-does-not-exist
    "")
  
  (function target-already-exists
    "")
  
  (function games/mods/files
    "")
  
  (function games/mods/files/get
    "")
  
  (function games/mods/files/add
    "")
  
  (function games/mods/files/edit
    "")
  
  (function games/mods/files/delete
    "")
  
  (function download-modfile
    "")
  
  (function extract-modfile
    ""))

;; subscribe.lisp
(docs:define-docs
  (function games/mods/subscribe
    "")
  
  (function games/mods/unsubscribe
    ""))

;; comments.lisp
(docs:define-docs
  (function games/mods/comments
    "")
  
  (function games/mods/comments/get
    "")
  
  (function games/mods/comments/add
    "")
  
  (function games/mods/comments/edit
    "")
  
  (function games/mods/comments/delete
    ""))

;; media.lisp
(docs:define-docs
  (function games/media/add
    "")
  
  (function games/mods/media/add
    "")
  
  (function games/mods/media/delete
    ""))

;; events.lisp
(docs:define-docs
  (function games/mods/events
    "")
  
  (function games/mods/mod-events
    ""))

;; tags.lisp
(docs:define-docs
  (function games/mods/tags
    "")
  
  (function games/mods/tags/add
    "")
  
  (function games/mods/tags/delete
    "")
  
  (function games/tags
    "")
  
  (function games/tags/add
    "")
  
  (function games/tags/delete
    ""))

;; ratings.lisp
(docs:define-docs
  (function games/mods/ratings/add
    ""))

;; stats.lisp
(docs:define-docs
  (function games/mods/stats/all
    "")
  
  (function games/mods/stats
    "")
  
  (function games/stats
    ""))

;; metadata.lisp
(docs:define-docs
  (function games/mods/metadata
    "")
  
  (function games/mods/metadata/add
    "")
  
  (function games/mods/metadata/delete
    ""))

;; dependencies.lisp
(docs:define-docs
  (function games/mods/dependencies
    "")
  
  (function games/mods/dependencies/add
    "")
  
  (function games/mods/dependencies/delete
    ""))

;; teams.lisp
(docs:define-docs
  (function games/mods/team
    "")
  
  (function games/mods/team/add
    "")
  
  (function games/mods/team/edit
    "")
  
  (function games/mods/team/delete
    ""))

;; general.lisp
(docs:define-docs
  (function general/ownership
    ""))

;; reports.lisp
(docs:define-docs
  (function report
    ""))

;; simple.lisp
(docs:define-docs
  (type simple-client
    "")
  
  (function mods-directory
    "")
  
  (function modlist-file
    "")
  
  (function modfile-cache-directory
    "")
  
  (function mod-directory
    "")
  
  (function load-local-modlist
    "")
  
  (function write-local-modlist
    "")
  
  (function load-remote-modlist
    "")
  
  (function extract-modfile
    "")
  
  (function find-modfile
    "")
  
  (function sync-modlist-from-remote
    "")
  
  (function sync-modlist-to-remote
    "")
  
  (function update-local-mods
    "")
  
  (function determine-mod-properties
    ""))
