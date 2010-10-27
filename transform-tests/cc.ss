#!r6rs

;; language: common

(import (rnrs)
        (transforms utils)
        (transforms cps)
        (transforms assignment)
        (transforms letrec-to-set)
        (transforms cc)
        (transform-tests utils)
        (transform-tests common))

(define (cc-check a b)
   (cond [(and (vector? a) (procedure? b)) #t]
         [(and (pair? a) (pair? b)) (and (cc-check (car a) (car b))
                                         (cc-check (cdr a) (cdr b)))]
         [else (equal? a b)]))

(define derived-tests
  (append (map cps-transform (append common-tests begin-tests))
          (map (compose cps-transform assignment-transform) set-tests)
          (map (compose cps-transform assignment-transform letrec-to-set) letrec-tests)))

(run-tests cc-transform
           cc-check
           (append common-tests
                   apply-tests
                   derived-tests))
