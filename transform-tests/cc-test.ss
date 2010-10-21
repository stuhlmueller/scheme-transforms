#!r6rs

(import (rnrs)
        (_srfi :1)
        (transforms cc)
        (transform-tests utils)
        (transform-tests tests)
        (only (ikarus) eval environment pretty-print))

(define (run-cc-test expr)
  (let* ([cc-e (cc-transform expr)]
         [cc-res (cc-eval expr)]
         [res (eval expr (environment '(rnrs)))]
         [test-passed (value-equal? cc-res res)])
    (pretty-print expr)
    (for-each display (list "cc result:  " cc-res "\n"
                            "base result: " res "\n"
                            (if test-passed "test passed" "test FAILED")
                            "\n"))
    (when (not test-passed) (pretty-print cc-e))
    (display "\n")))

(for-each run-cc-test (append common-test-exprs
                              cc-test-exprs))