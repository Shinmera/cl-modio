(asdf:defsystem cl-modio
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A client library for the mod.io API."
  :homepage "https://shinmera.com/project/cl-modio"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "client")
               (:file "objects")
               (:file "cache")
               (:file "authentication")
               (:file "me")
               (:file "games")
               (:file "mods")
               (:file "files")
               (:file "subscribe")
               (:file "comments")
               (:file "media")
               (:file "events")
               (:file "tags")
               (:file "ratings")
               (:file "stats")
               (:file "metadata")
               (:file "dependencies")
               (:file "teams")
               (:file "general")
               (:file "reports")
               (:file "simple")
               (:file "documentation"))
  :depends-on (:alexandria
               :uiop
               :zippy
               :com.inuoe.jzon
               :cl-ppcre
               :drakma
               :documentation-utils
               :language-codes))
