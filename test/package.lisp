;;;; Package definitions

(in-package #:cl-user)

(defpackage #:argon2/tests
  (:add-use-defaults t)
  (:use #:argon2)
  (:import-from #:parachute-browser #:define-test)
  (:import-from
   #:parachute
   #:test #:fail #:is #:isnt #:is-values #:isnt-values #:of-type #:finish)
  (:shadowing-import-from #:parachute #:true #:false)
  (:export #:run-all-tests))
