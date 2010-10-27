#!r6rs

;; language:
;; common + begin + set!

(import (rnrs)
        (_srfi :1)
        (transforms utils)
        (transforms assignment)
        (transforms letrec-to-set)
        (transform-tests utils)
        (transform-tests common))

(define derived-tests
  (map letrec-to-set letrec-tests))

(run-tests assignment-transform
           base-check
           (append common-tests
                   begin-tests
                   set-tests
                   derived-tests))
