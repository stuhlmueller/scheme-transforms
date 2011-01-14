#!r6rs

;; language:
;; common + begin

(import (rnrs)
        (scheme-tools srfi-compat :1)
        (transforms utils)
        (transforms cps)
        (transforms letrec-to-set)
        (transforms assignment)
        (transform-tests utils)
        (transform-tests common))

(define derived-tests
  (append (map (compose assignment-transform letrec-to-set) letrec-tests)
          (map assignment-transform set-tests)))

(run-tests cps-transform
           base-check
           (append common-tests
                   begin-tests
                   derived-tests))