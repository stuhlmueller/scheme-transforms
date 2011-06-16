#!r6rs

;; language:
;; common + begin + set!

(library

 (transform-tests assignment)

 (export run-assignment-tests)

 (import (rnrs)
         (scheme-tools srfi-compat :1)
         (transforms utils)
         (transforms assignment)
         (transforms letrec-to-set)
         (transform-tests utils)
         (transform-tests common))

 (define derived-tests
   (map letrec-to-set letrec-tests))

 (define (run-assignment-tests)
   (run-tests assignment-transform
              base-check
              evaluate
              (append common-tests
                      begin-tests
                      set-tests
                      derived-tests))))
 )