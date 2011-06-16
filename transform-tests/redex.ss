#!r6rs

;; language: common

(library

 (transform-tests redex)

 (export run-redex-tests)

 (import (rnrs)
         (transforms utils)
         (transforms cps)
         (transforms assignment)
         (transforms letrec-to-set)
         (transforms redex)
         (transforms untag)
         (transform-tests utils)
         (transform-tests common))

 (define derived-tests
   (append (map cps-transform (append common-tests begin-tests))
           (map (compose cps-transform assignment-transform) set-tests)
           (map (compose cps-transform assignment-transform letrec-to-set) letrec-tests)))

 (define (run-redex-tests)
   (run-tests redex-transform
              base-check
              app-evaluate
              (append common-tests
                      apply-tests
                      derived-tests)))

 )
