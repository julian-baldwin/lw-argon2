;;;; Argon2 FLI definitions

(in-package #:argon2)

(fli:define-foreign-variable
    (clear-internal-memory "FLAG_clear_internal_memory" :source)
  :type :int)

(fli:define-c-enum (argon2-error-code (:foreign-name "Argon2_ErrorCodes"))
  (argon2-ok 0)
  (argon2-output-ptr-null -1)
  (argon2-output-too-short -2)
  (argon2-output-too-long -3)
  (argon2-pwd-too-short -4)
  (argon2-pwd-too-long -5)
  (argon2-salt-too-short -6)
  (argon2-salt-too-long -7)
  (argon2-ad-too-short -8)
  (argon2-ad-too-long -9)
  (argon2-secret-too-short -10)
  (argon2-secret-too-long -11)
  (argon2-time-too-small -12)
  (argon2-time-too-large -13)
  (argon2-memory-too-little -14)
  (argon2-memory-too-much -15)
  (argon2-lanes-too-few -16)
  (argon2-lanes-too-many -17)
  (argon2-pwd-ptr-mismatch -18)
  (argon2-salt-ptr-mismatch -19)
  (argon2-secret-ptr-mismatch -20)
  (argon2-ad-ptr-mismatch -21)
  (argon2-memory-allocation-error -22)
  (argon2-free-memory-cbk-null -23)
  (argon2-allocate-memory-cbk-null -24)
  (argon2-incorrect-parameter -25)
  (argon2-incorrect-type -26)
  (argon2-out-ptr-mismatch -27)
  (argon2-threads-too-few -28)
  (argon2-threads-too-many -29)
  (argon2-missing-args -30)
  (argon2-encoding-fail -31)
  (argon2-decoding-fail -32)
  (argon2-thread-fail -33)
  (argon2-decoding-length-fail -34)
  (argon2-verify-mismatch -35))

(fli:define-c-enum (argon2-type (:foreign-name "Argon2_type"))
  (argon2-d 0)
  (argon2-i 1)
  (argon2-id 2))

(fli:define-c-typedef (argon2-type (:foreign-name "argon2_type"))
    (:enum argon2-type))

(fli:define-c-typedef (argon2-error-code (:foreign-name "argon2_error_codes"))
  (:enum argon2-error-code))

(fli:define-foreign-function (argon2i-hash-encoded "argon2i_hash_encoded" :source)
    ((t-cost (:const :uint32))
     (m-cost (:const :uint32))
     (parallelism (:const :uint32))
     (pwd (:pointer (:const :void)))
     (pwdlen (:const :size-t))
     (salt (:pointer (:const :void)))
     (saltlen (:const :size-t))
     (hashlen (:const :size-t))
     (encoded (:pointer :char))
     (encodedlen (:const :size-t)))
  :result-type argon2-error-code
  :language :ansi-c)

(fli:define-foreign-function (argon2d-hash-encoded "argon2d_hash_encoded" :source)
    ((t-cost (:const :uint32))
     (m-cost (:const :uint32))
     (parallelism (:const :uint32))
     (pwd (:pointer (:const :void)))
     (pwdlen (:const :size-t))
     (salt (:pointer (:const :void)))
     (saltlen (:const :size-t))
     (hashlen (:const :size-t))
     (encoded (:pointer :char))
     (encodedlen (:const :size-t)))
  :result-type argon2-error-code
  :language :ansi-c)

(fli:define-foreign-function (argon2id-hash-encoded "argon2id_hash_encoded" :source)
    ((t-cost (:const :uint32))
     (m-cost (:const :uint32))
     (parallelism (:const :uint32))
     (pwd (:pointer (:const :void)))
     (pwdlen (:const :size-t))
     (salt (:pointer (:const :void)))
     (saltlen (:const :size-t))
     (hashlen (:const :size-t))
     (encoded (:pointer :char))
     (encodedlen (:const :size-t)))
  :result-type argon2-error-code
  :language :ansi-c)

(fli:define-foreign-function (argon2-verify "argon2_verify" :source)
    ((encoded (:reference-pass :ef-mb-string))
     (pwd (:pointer (:const :void)))
     (pwdlen (:const :size-t))
     (type argon2-type))
  :result-type argon2-error-code
  :language :ansi-c)

(fli:define-foreign-function (argon2-error-message-fli "argon2_error_message" :source)
    ((error-code argon2-error-code))
  :result-type (:pointer (:const :char))
  :language :ansi-c)

(fli:define-foreign-function (argon2-encodedlen "argon2_encodedlen" :source)
    ((t-cost :uint32)
     (m-cost :uint32)
     (parallelism :uint32)
     (saltlen :uint32)
     (hashlen :uint32)
     (type argon2-type))
  :result-type :size-t
  :language :ansi-c)
