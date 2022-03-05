;;;; LispWorks Argon2 system definition

(asdf:defsystem "lw-argon2"
    :version "0.1"
    :author "Julian Baldwin"
    :license "MIT"
    :description "Simplified FLI to Argon2 with optional embedding."
    :components
    ((:module "src"
      :components ((:file "package")
                   #+argon2-embed
                   (:file "embed" :depends-on ("package"))))))
