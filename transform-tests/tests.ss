#!r6rs

(import (rnrs)
        (scheme-tools)
        (scheme-tools srfi-compat :1)
        (transform-tests letrec-to-set)
        (transform-tests assignment)
        (transform-tests cps)
        (transform-tests cc)
        (transform-tests redex))

(define all-tests
  (list (pair "Letrec-to-set tests" run-letrec-to-set-tests)
        (pair "Assignment tests" run-assignment-tests)
        (pair "Continuation-passing style tests" run-cps-tests)
        (pair "Redex transform tests" run-redex-tests)
        (pair "Closure-conversion tests" run-cc-tests)))

(define test->name first)
(define test->func rest)
(define stats->passed first)
(define stats->total second)

(define (run-all-tests)
  (let loop ([total 0]
             [passed 0]
             [tests all-tests])
    (if (null? tests)
        (begin
          (pe "\n# Summary\n")
          (if (= passed total)
              (pe "\nAll " total " tests PASSED.\n")
              (pe "\n" (- total passed) " out of " total " tests FAILED.\n")))
        (begin (pe "\n# " (test->name (first tests)) "\n\n")
               (let ([stats ((test->func (first tests)))])
                 (loop (+ total (stats->total stats))
                       (+ passed (stats->passed stats))
                       (rest tests)))))))

(run-all-tests)