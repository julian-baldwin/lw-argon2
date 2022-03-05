;;;; Smoke tests for Argon2

(in-package #:argon2/tests)

(defun run-all-tests ()
  "Run basic sanity tests."
  (parachute:test (find-package '#:argon2/tests) :report 'parachute::quiet))

(define-test argon2)

(defparameter +test-vectors+
  '((argon2-id 2 16 1 "password" "somesalt" "$argon2id$v=19$m=65536,t=2,p=1$c29tZXNhbHQ$CTFhFdXPJO1aFaMaO6Mm5c8y7cJHAph8ArZWb2GRPPc")
    (argon2-id 2 18 1 "password" "somesalt" "$argon2id$v=19$m=262144,t=2,p=1$c29tZXNhbHQ$eP4eyR+zqlZX1y5xCFTkw9m5GYx0L5YWwvCFvtlbLow")
    (argon2-id 2  8 1 "password" "somesalt" "$argon2id$v=19$m=256,t=2,p=1$c29tZXNhbHQ$nf65EOgLrQMR/uIPnA4rEsF5h7TKyQwu9U1bMCHGi/4")
    (argon2-id 2  8 2 "password" "somesalt" "$argon2id$v=19$m=256,t=2,p=2$c29tZXNhbHQ$bQk8UB/VmZZF4Oo79iDXuL5/0ttZwg2f/5U52iv1cDc")
    (argon2-id 1 16 1 "password" "somesalt" "$argon2id$v=19$m=65536,t=1,p=1$c29tZXNhbHQ$9qWtwbpyPd3vm1rB1GThgPzZ3/ydHL92zKL+15XZypg")
    (argon2-id 4 16 1 "password" "somesalt" "$argon2id$v=19$m=65536,t=4,p=1$c29tZXNhbHQ$kCXUjmjvc5XMqQedpMTsOv+zyJEf5PhtGiUghW9jFyw")
    (argon2-id 2 16 1 "differentpassword" "somesalt" "$argon2id$v=19$m=65536,t=2,p=1$c29tZXNhbHQ$C4TWUs9rDEvq7w3+J4umqA32aWKB1+DSiRuBfYxFj94")
    (argon2-id 2 16 1 "password" "diffsalt" "$argon2id$v=19$m=65536,t=2,p=1$ZGlmZnNhbHQ$vfMrBczELrFdWP0ZsfhWsRPaHppYdP3MVEMIVlqoFBw")
    (argon2-i  2 16 1 "password" "somesalt" "$argon2i$v=19$m=65536,t=2,p=1$c29tZXNhbHQ$wWKIMhR9lyDFvRz9YTZweHKfbftvj+qf+YFY4NeBbtA")
    (argon2-i  2 20 1 "password" "somesalt" "$argon2i$v=19$m=1048576,t=2,p=1$c29tZXNhbHQ$0Vh6ygkiw7XWqD7asxvuPE667zQu1hJ6VdGbI1GtH0E")
    (argon2-i  2 18 1 "password" "somesalt" "$argon2i$v=19$m=262144,t=2,p=1$c29tZXNhbHQ$KW266AuAfNzqrUSudBtQbxTbCVkmexg7EY+bJCKbx8s")
    (argon2-i  2  8 1 "password" "somesalt" "$argon2i$v=19$m=256,t=2,p=1$c29tZXNhbHQ$iekCn0Y3spW+sCcFanM2xBT63UP2sghkUoHLIUpWRS8")
    (argon2-i  2  8 2 "password" "somesalt" "$argon2i$v=19$m=256,t=2,p=2$c29tZXNhbHQ$T/XOJ2mh1/TIpJHfCdQan76Q5esCFVoT5MAeIM1Oq2E")
    (argon2-i  1 16 1 "password" "somesalt" "$argon2i$v=19$m=65536,t=1,p=1$c29tZXNhbHQ$0WgHXE2YXhPr6uVgz4uUw7XYoWxRkWtvSsLaOsEbvs8")
    (argon2-i  4 16 1 "password" "somesalt" "$argon2i$v=19$m=65536,t=4,p=1$c29tZXNhbHQ$qqlT1YrzcGzj3xrv1KZKhOMdf1QXUjHxKFJZ+IF0zls")
    (argon2-i  2 16 1 "differentpassword" "somesalt" "$argon2i$v=19$m=65536,t=2,p=1$c29tZXNhbHQ$FK6NoBr+qHAMI1jc73xTWNkCEoK9iGY6RWL1n7dNIu4")
    (argon2-i  2 16 1 "password" "diffsalt" "$argon2i$v=19$m=65536,t=2,p=1$ZGlmZnNhbHQ$sDV8zPvvkfOGCw26RHsjSMvv7K2vmQq/6cxAcmxSEnE"))
  "Argon2 test vectors reproduced from the C source.")

(defparameter +test-failures+
  '(("$argon2i$m=65536,t=2,p=1c29tZXNhbHQ$9sTbSlTio3Biev89thdrlKKiCaYsjjYVJxGAL3swxpQ" argon2::argon2-decoding-fail)
    ("$argon2i$m=65536,t=2,p=1$c29tZXNhbHQ9sTbSlTio3Biev89thdrlKKiCaYsjjYVJxGAL3swxpQ" argon2::argon2-decoding-fail)
    ("$argon2i$m=65536,t=2,p=1$$9sTbSlTio3Biev89thdrlKKiCaYsjjYVJxGAL3swxpQ" argon2::argon2-salt-too-short)
    ("$argon2i$m=65536,t=2,p=1$c29tZXNhbHQ$b2G3seW+uPzerwQQC+/E1K50CLLO7YXy0JRcaTuswRo" argon2::argon2-verify-mismatch))
  "Failure test cases reproduced from the C source.")

(defun test-encoding (type time memory threads password salt expected)
  (let ((salt (ef:encode-lisp-string salt :ascii))
        encoded)
    (finish (setf encoded (funcall 'hash-password password :salt salt :type type :iterations time :memory memory :threads threads)))
    (is equal expected encoded)
    (finish (verify-password password encoded))))

(defun test-failure (hash expected)
  (handler-case
      (progn
        (verify-password "password" hash)
        (true nil "Failed to signal exception for expected error ~A." expected))
    (argon2-error (condition)
      (is eql expected (argon2-error-code condition)))))

(define-test known-vectors :parent argon2
  (dolist (vector +test-vectors+)
    (apply 'test-encoding vector)))

(define-test failure-detection :parent argon2
  (dolist (failure +test-failures+)
    (apply 'test-failure failure)))
