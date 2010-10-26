#!r6rs

(library

 (transform-tests utils)

 (export run-tests)

 (import (rnrs)
         (transforms utils)
         (only (ikarus) eval environment pretty-print format)
         (_srfi :1))

 (define tests-failed '())

 (define (run-tests transformer evaluator checker tests)
   (set! tests-failed '())
   (for-each (curry run-test transformer evaluator checker)
             tests)
   (if (null? tests-failed)
       (display "\nall tests PASSED\n")
       (begin
         (display (format "\n~s FAILED tests:\n" (length tests-failed)))
         (map pretty-print tests-failed))))

 (define (run-test transformer evaluator checker expr)
   (let* ([test-e (transformer expr)]
          [test-res (evaluator expr)]
          [res (eval expr (environment '(rnrs)))]
          [test-passed (checker test-res res)])
     (display "Running Test:\n")
     (pretty-print expr)
     (for-each display (list "test result:  " test-res "\n"
                             "base result: " res "\n"
                             (if test-passed "test passed" "test FAILED")
                             "\n"))
     (when (not test-passed)
           (display "converted expr:\n")
           (pretty-print test-e)
           (set! tests-failed (pair expr tests-failed)))
     (display "\n")))

 )