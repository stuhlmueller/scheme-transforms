#!r6rs

;; language:
;; common + begin + set! + letrec

(library

 (transform-tests letrec-to-set)

 (export run-letrec-to-set-tests)

 (import (rnrs)
         (scheme-tools srfi-compat :1)
         (transforms utils)
         (transforms letrec-to-set)
         (transform-tests utils)
         (transform-tests common))

 (define (run-letrec-to-set-tests)
   (run-tests letrec-to-set
              base-check
              evaluate
              (append common-tests
                      begin-tests
                      set-tests
                      letrec-tests)))

 )