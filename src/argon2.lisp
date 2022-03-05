;;;; Argon2 FLI implementation

(in-package #:argon2)

;;; Parameter defaults

(defparameter *default-type* 'argon2-id "Default hash type.")
(defparameter *default-iterations* 3 "Default number of iterations to use.")
(defparameter *default-memory* 12 "Default memory usage (2^N KiB)")
(defparameter *default-threads* 1 "Default number of threads.")
(defparameter *default-hash-length* 32 "Default hash length in bytes.")
(defparameter *default-salt-length* 16 "Default salt length in bytes.")

(define-condition argon2-error (error)
  ((code :initarg :code
         :reader argon2-error-code
         :documentation "Underlying error code from the Argon2 library."))
  (:report (lambda (c s)
             (format s "Argon2 error: ~A - ~A."
                     (argon2-error-code c)
                     (argon2-error-message c)))
   (:documentation "Base class for all Argon2 error conditions.")))

(defun argon2-error-message (condition)
  "Return the Argon2 error message for a condition."
  (fli:convert-from-foreign-string (argon2-error-message-fli (argon2-error-code condition))))

(define-condition incorrect-password (argon2-error) ()
  (:report "The password is incorrect.")
  (:documentation "Subtype of ARGON2-ERROR for an incorrect password.")
  (:default-initargs :code 'argon2-verify-mismatch))

(defun check-argon2 (result)
  (or (eq result 'argon2-ok)
      (if (eq result 'argon2-verify-mismatch)
          (error 'incorrect-password)
          (error 'argon2-error :code result))))

#+(or darwin linux)
(defun random-data (length)
  "Generate a vector of LENGTH random bytes."
  (let ((result (make-array length :element-type '(unsigned-byte 8) :single-thread t)))
    (with-open-file (source "/dev/urandom" :direction :input :element-type '(unsigned-byte 8))
      (read-sequence result source)
      result)))

#+mswindows
(defun random-data (length)
  "Generate a vector of LENGTH random bytes."
  (declare (ignore length))
  (error "RANDOM-DATA not implemented on Windows."))

(defun hash-password (password
                      &key salt (salt-length (or (and salt (length salt)) *default-salt-length*))
                        (type *default-type*) (iterations *default-iterations*)
                        (memory *default-memory*) (threads *default-threads*)
                        (hash-length *default-hash-length*))
  "Hash a password string using Argon2, returning the encoded hash as a string."
  (let* ((memory (expt 2 memory))
         (encoded-length (argon2-encodedlen iterations memory threads salt-length hash-length type)))
    (fli:with-dynamic-foreign-objects
        ((salt-data (:unsigned :byte) :nelems salt-length)
         (hash-data :char :nelems encoded-length))
      (fli:replace-foreign-array salt-data
                                 (if salt
                                     (coerce salt '(vector (unsigned-byte 8)))
                                     (random-data salt-length))
                                 :end1 salt-length)
      (multiple-value-bind (password-data password-count password-length)
          (fli:convert-to-dynamic-foreign-string password :null-terminated-p nil)
        (declare (ignore password-count))
        (check-argon2 (funcall (ecase type
                                 ('argon2-i 'argon2i-hash-encoded)
                                 ('argon2-d 'argon2d-hash-encoded)
                                 ('argon2-id 'argon2id-hash-encoded))
                               iterations memory threads
                               password-data password-length
                               salt-data salt-length
                               hash-length
                               hash-data encoded-length))
        (fli:convert-from-foreign-string hash-data)))))

(defun verify-password (password encoded-hash &key (errorp t) type)
  "Verify a password against an encoded Argon2 hash, optionally signalling on failure."
  (fli:with-dynamic-foreign-objects ()
    (multiple-value-bind (password-data password-count password-length)
        (fli:convert-to-dynamic-foreign-string password :null-terminated-p nil)
      (declare (ignore password-count))
      (let ((result (argon2-verify encoded-hash password-data password-length
                                   (cond
                                     (type type)
                                     ((search "$argon2id$" encoded-hash) 'argon2-id)
                                     ((search "$argon2i$" encoded-hash) 'argon2-i)
                                     ((search "$argon2d$" encoded-hash) 'argon2-d)
                                     (t (error 'argon2-error :code 'argon2-decoding-fail))))))
        (or (eq result 'argon2-ok)
            (when errorp (check-argon2 result)))))))

(defun verify-no-login (&key (errorp t))
  "Perform a dummy password validation that always fails to defeat timing attacks."
  (hash-password "the quick brown fox jumps over the lazy dog")
  (when errorp (check-argon2 'argon2-verify-mismatch)))
