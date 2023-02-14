#|
 This file is a part of cl-modio
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.modio)

;; toolkit.lisp
(docs:define-docs
  (variable *portal*
    "Variable holding the current portal.

Should be one of: 
  :Apple
  :Discord
  :EGS
  :Facebook
  :GOG
  :Google
  :Itchio
  :Nintendo
  :OpenID
  :PSN
  :Steam
  :XboxLive
If NIL, the portal is auto-detected instead.

See DETECT-PORTAL")
  
  (function detect-platform
    "Returns the current platform we're on.

Should be one of:
  :windows
  :linux
  :mac
  :android
  :ios
  :xboxone
  :xboxseriesx
  :ps4
  :ps5
  :switch
  :wii")
  
  (function detect-portal
    "Returns the current portal we're connected to.

If *portal* is not NIL, returns *portal*. Otherwise if
cl-steamworks is loaded and available, :steam is returned.
Otherwise NIL is returned.

See *PORTAL*")

  (function f
    "Construct a filter from the given filtering expression.

EXPR      ::= NOT | SEARCH | BASIC
NOT       ::= (not SEARCH | BASIC)
SEARCH    ::= (search query)
BASIC     ::= (FILTER comparison PARAMETER)
FILTER    ::= equal | find | equalp | max | min | logand
PARAMETER --- The name of an API parameter.
"))

;; client.lisp
(docs:define-docs
  (variable *debug*
    "If T, request information is printed to *debug-io*

See CL:*DEBUG-IO*")
  
  (variable *client*
    "Holds the current CLIENT instance.

See CLIENT (type)")
  
  (variable *base-url*
    "Holds the base URL API requests are made against.

Defaults to https://api.mod.io/v1/")
  
  (type modio-condition
    "Base condition class for everything from this library.")
  
  (type request-error
    "Base error class for any request failure.

See MODIO-CONDITION
See DATA
See ENDPOINT
See ARGUMENTS
See ERROR-CODE
See MESSAGE")

  (function data
    "Returns the raw JSON data payload the failure returned, if any.

See REQUEST-ERROR")
  
  (function endpoint
    "Returns the endpoint that failed.

See REQUEST-ERROR")
  
  (function arguments
    "Returns the arguments that were passed to the endpoint that failed.

See REQUEST-ERROR")
  
  (function error-code
    "Returns the error-code that was returned for the failure.

See REQUEST-ERROR")
  
  (function message
    "Returns the message that was supplied about the failure.

See REQUEST-ERROR")
  
  (type bad-request
    "Error signalled on a malformed request.

See REQUEST-ERROR")
  
  (type invalid-access-key
    "Error signalled when the used access key is invalid.

See REQUEST-ERROR")
  
  (type permission-denied
    "Error signalled when attempting to access an endpoint without proper authorisation.

See REQUEST-ERROR")
  
  (type account-deleted
    "Error signalled when interacting from a deleted account.

See PERMISSION-DENIED")
  
  (type account-banned
    "Error signalled when interacting from a banned account.

See PERMISSION-DENIED")
  
  (type resource-not-found
    "Error signalled when interacting with an object that doesn't exist.

See REQUEST-ERROR")
  
  (type game-not-found
    "Error signalled when interacting with a game that doesn't exist.

See RESOURCE-NOT-FOUND")
  
  (type mod-not-found
    "Error signalled when interacting with a mod that doesn't exist.

See RESOURCE-NOT-FOUND")
  
  (type mod-deleted
    "Error signalled when interacting with a mod that has been deleted.

See RESOURCE-NOT-FOUND")
  
  (type modfile-not-found
    "Error signalled when interacting with a modfile that doesn't exist.

See RESOURCE-NOT-FOUND")
  
  (type comment-not-found
    "Error signalled when interacting with a comment that doesn't exist.

See RESOURCE-NOT-FOUND")
  
  (type user-not-found
    "Error signalled when interacting with a user that doesn't exist.

See RESOURCE-NOT-FOUND")
  
  (type resource-already-exists
    "Error signalled when attempting to submit an object with the same ID as one that already exists.

See REQUEST-ERROR")
  
  (type too-many-requests
    "Error signalled when too many requests have been made in a short time.

See REQUEST-ERROR")
  
  (type server-error
    "Error signalled when the remote server encountered an unexpected issue.

See REQUEST-ERROR")
  
  (type service-unavailable
    "Error signalled when the remote service is temporarily unavailable.

See REQUEST-ERROR")
  
  (function direct-request
    "Function used to create a request.

Handles data conversion and error conversion.

If PREPEND-BASE is T, *BASE-URL* is prepended to ENDPOINT.
If PARSE is T, the response is parsed as JSON and returned.
Otherwise the raw stream is returned.

See REQUEST-ERROR")
  
  (type client
    "Representation of a mod.io API client.

You should instantiate this with the correct API-KEY.

See *CLIENT*
See API-KEY
See ACCESS-TOKEN
See LANGUAGE
See PLATFORM
See PORTAL
See WAIT-UNTIL
See VALID-UNTIL
See ON-RATE-LIMIT
See CACHE
See DEFAULT-GAME-ID
See REQUEST
See REQUEST-LIST
See EXTRACT-USER-PROPERTIES
See RESTORE-USER-PROPERTIES")
  
  (function api-key
    "Accesses the API key by which the client connects to the API.

See CLIENT (type)")
  
  (function access-token
    "Accesses the access token by which the client connects to the API.

See CLIENT (type)")
  
  (function language
    "Accesses the language that is passed to the API.

This defaults to English. You should set it to the user's preferred language
to ensure you get messages from the API in that language.

See CLIENT (type)")
  
  (function platform
    "Accesses the platform that is passed to the API.

Defaults to DETECT-PLATFORM.

See DETECT-PLATFORM
See CLIENT (type)")
  
  (function portal
    "Accesses the portal that is passed to the API.

Defaults to DETECT-PORTAL

See DETECT-PORTAL
See CLIENT (type)")
  
  (function wait-until
    "Accesses the deadline until which requests are deferred.

Can be NIL or a universal-time timestamp.

See REQUEST
See CLIENT (type)")
  
  (function valid-until
    "Accesses the deadline until which the access-key is valid.

See ACCESS-KEY
See CLIENT (type)")
  
  (function on-rate-limit
    "Accesses the default action to perform when rate limitation is hit.

Can be one of:
  :RETURN             --- The REQUEST simply returns NIL
  :SLEEP              --- The thread is put to sleep until the deadline
  :ERROR              --- An error of TOO-MANY-REQUESTS is signalled
  function-designator --- The function is called

See REQUEST
See WAIT-UNTIL
See TOO-MANY-REQUESTS (type)
See CLIENT (type)")
  
  (function cache
    "Accesses the internal cache of the client.

See CACHE (type)
See CLIENT (type)")
  
  (function default-game-id
    "Accesses the default ID of the game you want to interact with.

See CLIENT (type)")

  (function authenticated-p
    "Returns true if the client is authenticated and the authentication token is still valid.

See CLIENT (type)")
  
  (function extract-user-properties
    "Returns a form that can be serialised to persist the client's current settings.

See RESTORE-USER-PROPERTIES
See CLIENT (type)")
  
  (function restore-user-properties
    "Restores the client's settings from the given form.

The form should be one that is EQUAL to one returned by EXTRACT-USER-PROPERTIES.

See EXTRACT-USER-PROPERTIES
See CLIENT (type)")
  
  (function request
    "Perform an API request from the client.

Handles rate limitation and appending all necessary headers and parameters
from the client for a proper API request. Otherwise same in function as
DIRECT-REQUEST

See ON-RATE-LIMIT
See DIRECT-REQUEST
See CLIENT (type)")
  
  (function request-list
    "Perform a listified request.

This calls REQUEST, but automatically re-calling REQUEST to traverse the
entire list of contents on the remote API endpoint. The following extra
arguments are available:

  :COLLECT-RESULTS --- If non-NIL, the results are accumulated and returned.
  :KEY       --- Function called for each object in the listed endpoint.
  :START     --- Start offset into the listed endpoint. Defaults to 0.
  :END       --- End limit of the listed endpoint. Defaults to infinity.
  :PER-PAGE  --- How many results to deliver per request. Defaults to 100.
  :SORT      --- By which attribute to sort the request. Can also be a
                 list of attribute and :ASC or :DESC to change direction.
  :FILTER    --- May be a list of filtering expressions to limit the results.
                 Use the F macro to construct a filter.

See F
See CLIENT (type)"))

;; objects.lisp
(docs:define-docs
  (type unique-resource
    "Base class for any object that has a unique ID.

See ID")
  
  (function id
    "Accesses the ID identifying the object on the API.

See UNIQUE-RESOURCE (type)")
  
  (type named-resource
    "Base class for any object that has a name and possibly name ID.

See NAME
See NAME-ID")
  
  (function name
    "Accesses the name of the object.

Note that this name is not necessarily unique.

see NAMED-RESOURCE (type)
See NAME-ID")
  
  (function name-id
    "Accesses the name-id of the object.

Unlike the NAME this is unique.

See NAMED-RESOURCE (type)
See NAME")

  (type mod-resource
    "Base class for any object that is a child of a mod.

See MOD-ID
See MOD")

  (function mod-id
    "Accesses the ID of the MOD this object is associated with.

See MOD-RESOURCE (type)")

  (function mod
    "Returns the MOD object this object is associated with.

See MOD (type)
See MOD-RESOURCE (type)")

  (type game-resource
    "Base class for any object that is a child of a game.

See GAME-ID")

  (function game-id
    "Accesses the ID of the GAME this object is associated with.

See GAME-RESOURCE (type)")

  (function game
    "Returns the GAME object this object is associated with.

See GAME (type)
See GAME-RESOURCE (type)")

  (type user-resource
    "Base class for any object that is a child of a user.

See USER-ID")

  (function user-id
    "Accesses the ID of the USER this object is associated with.

See USER-RESOURCE (type)")
  
  (type image
    "Representation of an image object.

See FILENAME
See ORIGINAL
See THUMBNAILS")
  
  (function file-name
    "Accesses the original file name of the object

See IMAGE (type)
See MODFILE (type)")
  
  (function original
    "Accesses the URL of the original image version.

See IMAGE (type)")
  
  (function thumbnails
    "Accesses the list of thumbnails for the image.

Returns a PLIST where the keys are resolution widths and the values
are URLs to images. May include the following keys:
  50 64 100 128 256 320 640 1280

See IMAGE (type)")
  
  (type comment
    "Represents a comment on a mod.

See UNIQUE-RESOURCE (type)
See MOD-RESOURCE (type)
See USER
See DATE-ADDED
See REPLY-ID
See THREAD-POSITION
See KARMA
See CONTENT
See REPLY")
  
  (function user
    "Returns the USER object associated with the object.

See USER (type)
See COMMENT (type)
See TEAM-MEMBER (type)")
  
  (function date-added
    "Returns the timestamp at which the object was added.

See COMMENT (type)
See GAME (type)
See MOD-DEPENDENCY (type)
See MOD-EVENT (type)
See MOD (type)
See MOD-TAG (type)
See MODFILE (type)
See RATING (type)
See USER-EVENT (type)")
  
  (function reply-id
    "Accesses the ID of the comment this is a reply to.

See COMMENT (type)")
  
  (function thread-position
    "Accesses the position of the comment in the thread.

See COMMENT (type)")
  
  (function karma
    "Accesses the karma (vote status) of the comment.

See COMMENT (type)")
  
  (function content
    "Accesses the text content of the comment.

See COMMENT (type)")
  
  (function reply
    "Returns the COMMENT object that this is a reply to, if any.

See COMMENT (type)")
  
  (type download
    "Representation of a download.

See BINARY-URL
See DATE-EXPIRES")
  
  (function binary-url
    "Accesses the URL at which the download can be accessed.

See DOWNLOAD (type)")
  
  (function date-expires
    "Accesses the timestamp after which the object is no longer valid.

See VALID-P
See DOWNLOAD (type)
See GAME-STATS (type)
See MOD-STATS (type)")
  
  (type game
    "Represents a game on the moddb registry.

See NAMED-RESOURCE (type)
See UNIQUE-RESOURCE (type)")
  
  (function status
    "Accesses the status of the object.

Can be one of:
  :UNACCEPTED
  :ACCEPTED
  :DELETED

See GAME (type)
See MOD (type)")
  
  (function submitted-by
    "Accesses the USER object that submitted this object.

See USER (type)
See GAME (type)
See MOD (type)")
  
  (function date-updated
    "Accesses the timestamp at which this object was last updated.

See GAME (type)
See MOD (type)")
  
  (function date-live
    "Accesses the timestamp at which this object was made available publicly.

See GAME (type)
See MOD (type)")
  
  (function presentation
    "Accesses the presentation option of the game.

Can be one of:
  :GRID
  :TABLE

See GAME (type)")
  
  (function submission
    "Accesses the submission option of the game.

Can be one of:
  :API
  :ANYWHERE

See GAME (type)")
  
  (function curation
    "Accesses the curation option of the game.

Can be one of:
  :NONE
  :PAID
  :FULL

See GAME (type)")
  
  (function community
    "Accesses the community options of the game.

Can be a list composed of:
  :COMMENTS
  :GUIDES
  :DISABLE-SUBSCRIBE-TEXT

See GAME (type)")
  
  (function revenue
    "Accesses the revenue options of the game.

Can be a list composed of:
  :SELLING-ALLOWED
  :DONATIONS-ALLOWED
  :TRADING-ALLOWED
  :SCARCITY-CONTROL-ALLOWED

See GAME (type)")
  
  (function api-access
    "Accesses the api-access options of the game.

Can be a list composed of:
  :3RD-PARTIES-ALLOWED
  :DIRECT-DOWNLOAD-ALLOWED

See GAME (type)")
  
  (function maturity
    "Accesses the maturity options of the game.

Can be a list composed of:
  :ALLOWED

See GAME (type)")
  
  (function ugc-name
    "Accesses the UGC type name of the game.

See GAME (type)")
  
  (function icon
    "Accesses the icon IMAGE of the game.

See GAME (type)
See IMAGE (type)")
  
  (function logo
    "Accesses the logo IMAGE of the object.

See GAME (type)
See MOD (type)
See IMAGE (type)")
  
  (function header
    "Accesses the header IMAGE of the game.

See GAME (type)
See IMAGE (type)")
  
  (function summary
    "Accesses the short summary text of the object.

See GAME (type)
See MOD (type)")
  
  (function instructions
    "Accesses the text to display for instructions on user contributions.

See GAME (type)")
  
  (function instructions-url
    "Accesses the URL to display for further user instructions.

See GAME (type)")
  
  (function profile-url
    "Accesses the URL at which the object can be accessed canonically.

See GAME (type)
See MOD (type)
See USER (type)")
  
  (function stats
    "Accesses the statistical data of the object.

See GAME (type)
See MOD (type)")
  
  (function tag-options
    "Accesses the tag options of the game

See GAME (type)
See GAME-TAG-OPTION (type)")
  
  (type game-stats
    "Represents statistics about a game.

See GAME-RESOURCE (type)
See MODS
See DOWNLOADS
See SUBSCRIBERS
See DATE-EXPIRES")
  
  (function mods
    "Accesses the total number of mods available for the game.

See GAME-STATS (type)")
  
  (function downloads
    "Accesses the download statistics of the object.

Should be a plist with the following keys:
  :TODAY
  :TOTAL
  :DAILY

See GAME-STATS (type)
See MOD-STATS (type)")
  
  (function subscribers
    "Accesses the total number of subscribers of the object.

See GAME-STATS (type)
See MOD-STATS (type)")
  
  (type game-tag-option
    "Represents a tagging option for the game.

See NAMED-RESOURCE (type)
See TAG-TYPE
See TAGS
See TAG-COUNT
See HIDDEN
See LOCKED")
  
  (function tag-type
    "Accesses the type of tag category this is.

Can be one of:
  :CHECKBOXES
  :DROPDOWN

See GAME-TAG-OPTION (type)")
  
  (function tags
    "Accesses the list of actual tags of the object.

See GAME-TAG-OPTION (type)
See MOD (type)")
  
  (function tag-count
    "Accesses the number of tags in the category.

See GAME-TAG-OPTION (type)")
  
  (function hidden
    "Accesses whether the category should be hidden or not.

See GAME-TAG-OPTION (type)")
  
  (function locked
    "Accesses whether the category should be locked or not.

See GAME-TAG-OPTION (type)")
  
  (type mod-dependency
    "Represents a dependency of a mod on another.

See NAMED-RESOURCE (type)
See MOD-RESOURCE (type)
See DATE-ADDED")
  
  (type mod-event
    "Represents a change event on a mod.

See UNIQUE-RESOURCE (type)
See MOD-RESOURCE (type)
See USER-RESOURCE (type)
See DATE-ADDED
See EVENT-TYPE")
  
  (function event-type
    "Accesses the type of the event described.

Can be one of:
  :JOIN
  :LEAVE
  :SUBSCRIBE
  :UNSUBSCRIBE
  :FILE-CHANGED
  :AVAILABLE
  :UNAVAILABLE
  :EDITED
  :DELETED
  :TEAM-CHANGED

See MOD-EVENT (type)
See USER-EVENT (type)")
  
  (type mod-media
    "Representation of media associated with a mod.

See YOUTUBE-URLS
See SKETCHFAB-URLS
See IMAGES")
  
  (function youtube-urls
    "Accesses the list of YouTube video URLs for the mod.

See MOD-MEDIA (type)")
  
  (function sketchfab-urls
    "Accesses the list of Sketchfab video URLs for the mod.

See MOD-MEDIA (type)")
  
  (function images
    "Accesses the list of IMAGE objects for the mod.

See MOD-MEDIA (type)")
  
  (type mod
    "Representation of a game mod.

See NAMED-RESOURCE (type)
See UNIQUE-RESOURCE (type)
See GAME-RESOURCE (type)
See STATUS
See VISIBLE
See SUBMITTED-BY
See DATE-ADDED
See DATE-UPDATED
See DATE-LIVE
See MATURITY
See LOGO
See HOMEPAGE-URL
See SUMMARY
See DESCRIPTION
See DESCRIPTION-HTML
See MODFILE
See METADATA-BLOB
See PROFILE-URL
See MEDIA
See STATS
See METADATA
See TAGS")
  
  (function visible
    "Accesses the visibility of the mod.

Can be one of:
  :HIDDEN
  :PUBLIC

See MOD (type)")
  
  (function homepage-url
    "Accesses the upstream homepage URL of the mod.

See MOD (type)")
  
  (function description
    "Accesses the plaintext description of the mod.

See MOD (type)")
  
  (function description-html
    "Accesses the rich HTML text description of the mod.

See MOD (type)")
  
  (function modfile
    "Accesses the most recent modfile of the mod.

See MOD (type)
See MODFILE (type)")
  
  (function metadata-blob
    "Accesses the metadata blob of the object.

See MOD (type)
See MODFILE (type)")
  
  (function media
    "Accesses the media object of the mod.

See MOD (type)
See MOD-MEDIA (type)")
  
  (function metadata
    "Accesses the metadata table of the mod.

See MOD (type)")
  
  (type mod-stats
    "Representation of the statistics about a mod.

See MOD-RESOURCE (type)
See DATE-EXPIRES
See POPULARITY
See DOWNLOADS
See SUBSCRIBERS
See RATING")
  
  (function popularity
    "Accesses the popularity metrics plist.

Contains the following keys:
  :POSITION
  :TOTAL

See MOD-STATS (type)")
  
  (function rating
    "Accesses the rating of the object.

For the RATING this can be one of:
  :GOOD
  :NONE
  :BAD

For the MOD-STATS this is a plist containing the following keys:
  :TOTAL
  :POSITIVE
  :NEGATIVE
  :PERCENTAGE
  :AGGREGATE
  :TEXT

See MOD-STATS (type)
See RATING (type)")
  
  (type mod-tag
    "Representation of a tag on a mod.

See NAMED-RESOURCE (type)
See DATE-ADDED")

  (type modfile
    "Representation of a mod's payload.

See UNIQUE-RESOURCE (type)
See MOD-RESOURCE (type)
See DATE-ADDED
See DATE-SCANNED
See VIRUS-STATUS
See VIRUS-POSITIVE
See VIRUS-HASH
See FILE-SIZE
See FILE-HASH
See FILE-NAME
See VERSION
See CHANGELOG
See METADATA-BLOB
See DOWNLOAD")
  
  (function date-scanned
    "Accesses the timestamp at which the file was scanned.")
  
  (function virus-status
    "Accesses the virus scan's status.

Can be one of:
  :UNSCANNED
  :COMPLETED
  :IN-PROGRESS
  :TOO-LARGE
  :FILE-NOT-FOUND
  :SCANNING-ERROR

See MODFILE (type)")
  
  (function virus-positive
    "Accesses the virus scan's result.

Can be one of:
  :NO-THREATS
  :FLAGGED

See MODFILE (type)")
  
  (function virus-hash
    "Accesses the hash that the virus scanner produced.

See MODFILE (type)")
  
  (function file-size
    "Accesses the size of the mod payload in bytes.

See MODFILE (type)")
  
  (function file-hash
    "Accesses the MD5 hash of the mod payload.

See MODFILE (type)")
  
  (function file-name
    "Accesses the original file name of the mod payload.

See MODFILE (type)")
  
  (function version
    "Accesses the version string of the mod payload.

See MODFILE (type)")
  
  (function changelog
    "Accesses the changelog of the mod payload.

See MODFILE (type)")

  (function download
    "Accesses the download object of the modfile.

See MODFILE (type)
See DOWNLOAD (type)")
  
  (type rating
    "Representation of a rating on a game or mod.

See MOD-RESOURCE (type)
See GAME-RESOURCE (type)
See RATING
See DATE-ADDED")
  
  (type team-member
    "Representation of a team member.

See UNIQUE-RESOURCE (type)
See USER
See LEVEL
See DATE-ADDED
See TITLE
See INVITE")
  
  (function level
    "Accesses the level of the member.

Can be a list composed of:
  :MODERATOR
  :MANAGER
  :ADMINISTRATOR

See TEAM-MEMBER (type)")
  
  (function title
    "Accesses the member's official title.

See TEAM-MEMBER (type)")
  
  (function invite
    "Accesses the member's invite status.

Can be one of:
  :ACCEPTED
  :PENDING

See TEAM-MEMBER (type)")
  
  (type user-event
    "Represents a change event on a user.

See UNIQUE-RESOURCE (type)
See MOD-RESOURCE (type)
See GAME-RESOURCE (type)
See USER-RESOURCE (type)
See DATE-ADDED
See EVENT-TYPE")
  
  (type user
    "Represents a user.

See NAMED-RESOURCE (type)
See UNIQUE-RESOURCE (type)
See DISPLAY-NAME
See LAST-ONLINE
See AVATAR
See PROFILE-URL")
  
  (function display-name
    "Accesses the preferred display name of the user.

You should use this name instead of the raw account name to display to
other users.

See USER (type)")
  
  (function last-online
    "Accesses the timestamp at which the user was last seen online.

See USER (type)")
  
  (function avatar
    "Accesses the user's avatar IMAGE object.

See USER (type)
See IMAGE (type)"))

;; cache.lisp
(docs:define-docs
  (function valid-p
    "Returns true if the given object is still considered valid.

See DATE-EXPIRES")
  
  (function clear-cache
    "Clears the cache completely.

See CACHE (type)
See CLIENT (type)"))

;; authentication.lisp
(docs:define-docs
  (function complete-authentication
    "Function called when authentication completes successfully.

You may define secondary methods on this function to react to a
successful login.

See CLIENT (type)")
  
  (function authenticate/terms
    "Returns the authentication terms you must show to the user for third-party authentication.

See CLIENT (type)")
  
  (function authenticate/logout
    "Invalidate the access-token of the client and log out.

See CLIENT (type)")
  
  (function authenticate/email-request
    "Initiates an email login flow.

See CLIENT (type)")
  
  (function authenticate/email-exchange
    "Completes an email login flow.

See COMPLETE-AUTHENTICATION
See CLIENT (type)")
  
  (function authenticate/steam
    "Authenticate against steam using an encrypted app ticket.

See COMPLETE-AUTHENTICATION
See CLIENT (type)")
  
  (function authenticate/epic
    "Authenticate against epic using an access token.

See COMPLETE-AUTHENTICATION
See CLIENT (type)")
  
  (function authenticate/gog-galaxy
    "Authenticate against GOG Galaxy using the app data.

See COMPLETE-AUTHENTICATION
See CLIENT (type)")
  
  (function authenticate/itchio
    "Authenticate against Itch.io using an access token.

See COMPLETE-AUTHENTICATION
See CLIENT (type)")
  
  (function authenticate/oculus
    "Authenticate against Oculus with the given device identification.

See COMPLETE-AUTHENTICATION
See CLIENT (type)")
  
  (function authenticate/xbox
    "Authenticate against Xbox using the given auth token.

See COMPLETE-AUTHENTICATION
See CLIENT (type)")
  
  (function authenticate/switch
    "Authenticate against Switch Online using the given auth token.

See COMPLETE-AUTHENTICATION
See CLIENT (type)")
  
  (function authenticate/google
    "Authenticate against Google using the given auth token.

See COMPLETE-AUTHENTICATION
See CLIENT (type)")
  
  (function authenticate/discord
    "Authenticate against Discord using the given auth token.

See COMPLETE-AUTHENTICATION
See CLIENT (type)")
  
  (function authenticate/openid
    "Authenticate against an OpenID provider using the given id-token.

See COMPLETE-AUTHENTICATION
See CLIENT (type)"))

;; me.lisp
(docs:define-docs
  (function me
    "Returns the current user that is authenticated.

See CLIENT (type)
See USER (type)")
  
  (function me/subscribed
    "Returns a list of subscribed mods for the current user.

See CLIENT (type)
See MOD (type)")
  
  (function me/events
    "Returns a list of user events.

See CLIENT (type)
See USER-EVENT (type)")
  
  (function me/games
    "Returns a list of games the current user has contributed to.

See CLIENT (type)
See GAME (type)")
  
  (function me/mods
    "Returns a list of mods the current user has contributed to.

See CLIENT (type)
See MOD (type)")
  
  (function me/files
    "Returns a list of files the current user has contributed to.

See CLIENT (type)
See MODFILE (type)")
  
  (function me/ratings
    "Returns a list of ratings the current user has contributed.

See CLIENT (type)
See RATING (type)"))

;; games.lisp
(docs:define-docs
  (function games
    "Returns a list of matching games.

See GAME (type)
See CLIENT (type)")
  
  (function games/get
    "Returns the requested game object.

See GAME (type)
See CLIENT (type)")
  
  (function games/edit
    "Updates the given game object.

See GAME (type)
See CLIENT (type)"))

;; mods.lisp
(docs:define-docs
  (function games/mods
    "Returns a list of matching mods for the game.

See MOD (type)
See CLIENT (type)")
  
  (function games/mods/get
    "Returns the requested mod object.

See MOD (type)
See CLIENT (type)")
  
  (function games/mods/add
    "Creates a new mod object on the game.

See MOD (type)
See CLIENT (type)")
  
  (function games/mods/edit
    "Updates the given mod object.

See MOD (type)
See CLIENT (type)")
  
  (function games/mods/delete
    "Deletes the given mod object.

See MOD (type)
See CLIENT (type)"))

;; files.lisp
(docs:define-docs
  (type modfile-error
    "Base condition for all errors related to modfile management.

See MODIO-CONDITION (type)
See MODFILE
See TARGET")
  
  (function target
    "Returns the target path of the modfile operation.

See MODFILE-ERROR (type)")
  
  (type download-corrupted
    "Error signalled when the downloaded modfile does not match expected size and hash.

See FILE-SIZE
See MODFILE-ERROR (type)")
  
  (type target-does-not-exist
    "Error signalled when the modfile is not properly cached.

See CLIENT (type)")
  
  (type target-already-exists
    "Error signalled when the modfile is already cached.

See CLIENT (type)")
  
  (function games/mods/files
    "Returns a list of modfiles of the given mod.

See MODFILE (type)
See CLIENT (type)")
  
  (function games/mods/files/get
    "Returns the requested modfile object.

See MODFILE (type)
See CLIENT (type)")
  
  (function games/mods/files/add
    "Creates a new modfile object.

See MODFILE (type)
See CLIENT (type)")
  
  (function games/mods/files/edit
    "Updates the given modfile object.

See MODFILE (type)
See CLIENT (type)")
  
  (function games/mods/files/delete
    "Deletes the given modfile object.

See MODFILE (type)
See CLIENT (type)")
  
  (function download-modfile
    "Downloads the given modfile to disk.

IF-EXISTS may be the following:
  :SUPERSEDE  --- The file is overwritten if the remote modfile is newer.
  :OVERWRITE  --- The file is always overwritten.
  :ERROR      --- An error of type TARGET-ALREADY-EXISTS is signalled.
  :RETURN     --- The TARGET is returned with no modifications.
  NIL         --- NIL is returned with no modifications.

IF-DOES-NOT-EXIST may be the following:
  :CREATE     --- The file is downloaded.
  :ERROR      --- An error of type TARGET-DOES-NOT-EXIST is signalled.
  NIL         --- NIL is returned with no modifications.

See CLIENT (type)")
  
  (function extract-modfile
    "Extract the given modfile contents to disk.

First calls DOWNLOAD-MODFILE with :IF-EXISTS :SUPERSEDE and
:IF-DOES-NOT-EXIST IF-NO-CACHE.

IF-EXISTS may be one of the following:
  :SUPERSEDE  --- The target directory is deleted and the contents are
                  extracted anew. Note if you do not want it to delete
                  the directory, use :OVERWRITE instead.
  :OVERWRITE  --- Overwrite the target directory contents with the new
                  files, leaving old files intact.
  :ERROR      --- Signal an error of type TARGET-ALREADY-EXISTS.
  NIL         --- NIL is returned with no modifications.

See DOWNLOAD-MODFILE
See CLIENT (type)"))

;; subscribe.lisp
(docs:define-docs
  (function games/mods/subscribe
    "Subscribe the current user to the given mod.

See CLIENT (type)")
  
  (function games/mods/unsubscribe
    "Unsubscribe the current user from the given mod.

See CLIENT (type)"))

;; comments.lisp
(docs:define-docs
  (function games/mods/comments
    "Returns a list of COMMENT objects on the given mod.

See COMMENT (type)
See CLIENT (type)")
  
  (function games/mods/comments/get
    "Returns the requested comment.

See COMMENT (type)
See CLIENT (type)")
  
  (function games/mods/comments/add
    "Creates a new comment.

See COMMENT (type)
See CLIENT (type)")
  
  (function games/mods/comments/edit
    "Updates an existing comment.

See COMMENT (type)
See CLIENT (type)")
  
  (function games/mods/comments/delete
    "Deletes an existing comment.

See COMMENT (type)
See CLIENT (type)"))

;; media.lisp
(docs:define-docs
  (function games/media/add
    "Uploads a media file to a game.

See CLIENT (type)")
  
  (function games/mods/media/add
    "Uploads a new media file to a mod.

See CLIENT (type)")
  
  (function games/mods/media/delete
    "Deletes an existing media file from a mod.

See CLIENT (type)"))

;; events.lisp
(docs:define-docs
  (function games/mods/events
    "Returns a list of events for the game.

See GAME-EVENT (type)
See CLIENT (type)")
  
  (function games/mods/mod-events
    "Returns a list of events for the mod.

See MOD-EVENT (type)
See CLIENT (type)"))

;; tags.lisp
(docs:define-docs
  (function games/mods/tags
    "Returns a list of tags for the mod.

See MOD-TAG
See CLIENT (type)")
  
  (function games/mods/tags/add
    "Adds a new tag to the mod.

See CLIENT (type)")
  
  (function games/mods/tags/delete
    "Deletes an existing tag from a mod.

See CLIENT (type)")
  
  (function games/tags
    "Returns a list of tag options for the game.

See GAME-TAG-OPTION (type)
See CLIENT (type)")
  
  (function games/tags/add
    "Adds a new tag option to a game.

See CLIENT (type)")
  
  (function games/tags/delete
    "Deletes an existing tag option from a game.

See CLIENT (type)"))

;; ratings.lisp
(docs:define-docs
  (function games/mods/ratings/add
    "Adds or updates a rating on a mod.

See CLIENT (type)"))

;; stats.lisp
(docs:define-docs
  (function games/mods/stats/all
    "Returns a list of stats about the mods in the game

See MOD-STATS (type)
See CLIENT (type)")
  
  (function games/mods/stats
    "Returns the stats object for the given mod.

See MOD-STATS (type)
See CLIENT (type)")
  
  (function games/stats
    "Returns the stats object for the given game.

See GAME-STATS (type)
See CLIENT (type)"))

;; metadata.lisp
(docs:define-docs
  (function games/mods/metadata
    "Returns the table of metadata on the given mod.

See CLIENT (type)")
  
  (function games/mods/metadata/add
    "Adds or updates an existing metadata key on the given mod.

See CLIENT (type)")
  
  (function games/mods/metadata/delete
    "Deletes an existing metadata key from the given mod.

See CLIENT (type)"))

;; dependencies.lisp
(docs:define-docs
  (function games/mods/dependencies
    "Returns a list of dependencies for the given mod.

See MOD-DEPENDENCY (type)
See CLIENT (type)")
  
  (function games/mods/dependencies/add
    "Adds new dependencies to the given mod.

See CLIENT (type)")
  
  (function games/mods/dependencies/delete
    "Removes dependencies from the given mod.

See CLIENT (type)"))

;; teams.lisp
(docs:define-docs
  (function games/mods/team
    "Returns a list of all team members of the given mod.

See TEAM-MEMBER (type)
See CLIENT (type)")
  
  (function games/mods/team/add
    "Adds a new team member to the given mod.

See CLIENT (type)")
  
  (function games/mods/team/edit
    "Updates an existing team member in a mod.

See CLIENT (type)")
  
  (function games/mods/team/delete
    "Deletes an existing team member from a mod.

See CLIENT (type)"))

;; general.lisp
(docs:define-docs
  (function general/ownership
    "Returns the owner of the given object.

See USER (type)
See CLIENT (type)"))

;; reports.lisp
(docs:define-docs
  (function report
    "Submit a report about something that violates terms of service or rules.

See CLIENT (type)"))

;; simple.lisp
(docs:define-docs
  (type simple-client
    "Client that implements a simplified interface for mod management.

You must supply a DEFAULT-GAME-ID and MODS-DIRECTORY.

See CLIENT (type)
See MODS-DIRECTORY
See MODLIST-FILE
See MODFILE-CACHE-DIRECTORY
See MOD-DIRECTORY
See LOAD-LOCAL-MODLIST
See WRITE-LOCAL-MODLIST
See LOAD-REMOTE-MODLIST
See DOWNLOAD-MODFILE
See EXTRACT-MODFILE
See FIND-MODFILE
See SYNC-MODLIST-FROM-REMOTE
See SYNC-MODLIST-TO-REMOTE
See UPDATE-LOCAL-MODS
See DETERMINE-MOD-PROPERTIES")
  
  (function mods-directory
    "Accesses the path to the directory in which the client will manage mod data.

See SIMPLE-CLIENT (type)")
  
  (function modlist-file
    "Returns the file in which the list of mods is stored.

See SIMPLE-CLIENT (type)")
  
  (function modfile-cache-directory
    "Returns the directory in which modfiles are cached.

See SIMPLE-CLIENT (type)")
  
  (function mod-directory
    "Returns the directory in which the given mod's files are stored.

See SIMPLE-CLIENT (type)")
  
  (function load-local-modlist
    "Returns a list of mods in the local modlist.

See SIMPLE-CLIENT (type)")
  
  (function write-local-modlist
    "Writes the list of mods into the local modlist file.

See SIMPLE-CLIENT (type)")
  
  (function load-remote-modlist
    "Returns a list of mods the user is subscribed to.

See SIMPLE-CLIENT (type)")
  
  (function find-modfile
    "Attempt to find a matching remote modfile of the given specs.

See MODFILE (type)
See SIMPLE-CLIENT (type)")
  
  (function sync-modlist-from-remote
    "Synchronise the modlist with the remote.

This assumes the remote has the correct data. Deviations in the local
modlist are subject to the following:

IF-DOES-NOT-EXIST may be one of the following:
  :CREATE  --- The mod is downloaded and extracted.
  :ERROR   --- An error is signalled.
  NIL      --- Nothing is done.

IF-EXISTS may be one of the following:
  :DELETE     --- The mod's files are deleted.
  :DEACTIVATE --- The mod is marked as inactive.
  :ERROR      --- An error is signalled.
  NIL         --- Nothing is done.

After completing the sync, the local modlist is written.

See SIMPLE-CLIENT (type)")
  
  (function sync-modlist-to-remote
    "Synchronise the modlist with the remote.

This assumes the local modlist has the correct data.
All subscriptions are updated to reflect the current modlist.

See SIMPLE-CLIENT (type)")
  
  (function update-local-mods
    "Update all local mods if possible.

IF-EXISTS may be one of the following:
  :SUPERSEDE  --- The files are only updated if the remote is newer.
  :OVERWRITE  --- The files are updated unconditionally.

If-DOES-NOT-EXIST may be one of the following:
  :DELETE     --- The local files are deleted.
  :DEACTIVATE --- The mod is marked as inactive.
  :ERROR      --- An error is signalled.
  NIL         --- Nothing is done.

After completing the sync, the local modlist is written.

See SIMPLE-CLIENT (type)")
  
  (function determine-mod-properties
    "Attempt to discover information about the mod by parsing any ASD files within.

See SIMPLE-CLIENT (type)"))
