#!r6rs

;; language:
;; common + begin + set! + letrec

(import (rnrs)
        (scheme-tools srfi-compat :1)
        (transforms utils)
        (transforms letrec-to-set)
        (transform-tests utils)
        (transform-tests common))

(run-tests letrec-to-set
           base-check
           (append common-tests
                   begin-tests
                   set-tests
                   letrec-tests))
