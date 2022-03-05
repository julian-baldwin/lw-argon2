;;;; Package definitions.

(in-package #:cl-user)

(defpackage #:argon2
  (:add-use-defaults t)
  (:export
   #+argon2-embed
   #:initialise-embedded
   #:hash-password
   #:verify-password
   #:verify-no-login
   #:argon2-error
   #:argon2-error-code
   #:argon2-error-message
   #:incorrect-password
   #:argon2-i
   #:argon2-d
   #:argon2-id))
