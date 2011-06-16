#!r6rs

;; language: common

(library

 (transform-tests cc)

 (export run-cc-tests)

 (import (rnrs)
         (transforms utils)
         (transforms cps)
         (transforms assignment)
         (transforms letrec-to-set)
         (transforms cc)
         (transforms untag)
         (transform-tests utils)
         (transform-tests common))

 (define (cc-check a b)
   (cond [(and (vector? a) (procedure? b)) #t]
         [(and (pair? a) (pair? b)) (and (cc-check (car a) (car b))
                                         (cc-check (cdr a) (cdr b)))]
         [else (equal? a b)]))

 (define derived-tests
   (append (map (compose untag-transform cps-transform) (append common-tests begin-tests))
           (map (compose untag-transform cps-transform assignment-transform) set-tests)
           (map (compose untag-transform cps-transform assignment-transform letrec-to-set) letrec-tests)))

 (define (run-cc-tests)
   (run-tests cc-transform
              cc-check
              evaluate
              (append (map untag-transform common-tests)
                      (map untag-transform apply-tests)
                      derived-tests)))

 )
