;;;; Embeds the Argon2 reference implementation as a LispWorks foreign module.

(in-package #:argon2)

(eval-when (:load-toplevel :execute)
  (defparameter *build-path* (asdf:system-relative-pathname "lw-argon2" "build/"))
  (defparameter *source-path* (asdf:system-relative-pathname "lw-argon2" "deps/argon2/src/"))
  (defparameter *include-paths*
    (list *source-path*
          (asdf:system-relative-pathname "lw-argon2" "deps/argon2/include/")
          (asdf:system-relative-pathname "lw-argon2" "deps/argon2/src/blake2/"))))

(eval-when (:load-toplevel :execute)
  (let ((sources (list "argon2" "core" "thread" "encoding" "blake2/blake2b" "opt"))
        (output (make-pathname :name "argon2-concat" :type "c" :defaults *build-path*)))
    (delete-file output nil)
    (mapc (lambda (file)
            (format t "~&;; concatenating ~A~%" file)
            (append-file (make-pathname :name file :type "c" :defaults *source-path*) output))
          sources)
    (defsystem "lw-argon2/embed"
        (:default-pathname *build-path*)
        :members (("argon2-concat.c" :type :c-file :embedded-module argon2-embedded)))
    (let ((scm:*c-default-options*
            #+(or macosx linux) (apply 'string-append scm:*c-default-options* " -O3 "
                                       (mapcar (lambda (path) (format nil " -I ~A " (namestring path)))
                                               *include-paths*))
            #-(or macosx linux) (error "Compiler flags not implemented for this platform.")))
      (compile-system "lw-argon2/embed" :load t))))

(defun initialise-embedded ()
  "Install the embedded foreign module at runtime."
  (fli:install-embedded-module 'argon2-embedded))
