;;;; LispWorks Argon2 system definition

(asdf:defsystem "lw-argon2"
    :version "0.1"
    :author "Julian Baldwin"
    :license "MIT"
    :description "Simplified FLI to Argon2 with optional embedding."
    :in-order-to ((test-op (test-op "lw-argon2/tests")))
    :components
    ((:module "src"
      :components ((:file "package")
                   #+argon2-embed
                   (:file "embed" :depends-on ("package"))
                   (:file "fli" :depends-on ("package"))
                   (:file "argon2" :depends-on ("package"))))))

(asdf:defsystem "lw-argon2/tests"
    :version "0.1"
    :author "Julian Baldwin"
    :license "MIT"
    :description "Sanity tests for LW-Argon2."
    :depends-on ("parachute-browser")
    :perform (test-op (o s)
                      (symbol-call '#:argon2 '#:initialise-embedded)
                      (symbol-call '#:argon2/tests '#:run-all-tests))
    :components
    ((:module "test"
      :serial t
      :components ((:file "package")
                   (:file "argon2")))))
