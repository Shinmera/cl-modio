#|
 This file is a part of cl-modio
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem cl-modio
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A client library for the mod.io API."
  :homepage "https://github.com/Shinmera/cl-modio"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "client")
               (:file "authentication")
               (:file "documentation"))
  :depends-on (:yason
               :cl-ppcre
               :drakma
               :documentation-utils
               :language-codes))
