#!r6rs

(import (rnrs)
        (_srfi :1)
        (transforms cps)
        (transform-tests utils)
        (transform-tests tests)
        (only (ikarus) eval environment pretty-print))

(define (run-cps-test expr)
  (let* ([cps-e (cps-transform expr)]
         [cps-res (cps-eval expr)]
         [res (eval expr (environment '(rnrs)))]
         [test-passed (value-equal? cps-res res)])
    (pretty-print expr)
    (for-each display (list "cps result:  " cps-res "\n"
                            "base result: " res "\n"
                            (if test-passed "test passed" "test FAILED")
                            "\n"))
    (when (not test-passed) (pretty-print cps-e))
    (display "\n")))

(for-each run-cps-test (append common-test-exprs
                               cps-test-exprs))