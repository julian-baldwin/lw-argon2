;;;; Package definitions.

(in-package #:cl-user)

(defpackage #:argon2
  (:add-use-defaults t)
  (:export
   #+argon2-embed
   #:initialise-embedded))
