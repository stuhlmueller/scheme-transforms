#!r6rs

(library

 (transform-tests utils)

 (export value-equal?
         run-tests)

 (import (rnrs)
         (transforms utils)
         (only (ikarus) eval environment pretty-print)
         (_srfi :1))

 (define (value-equal? a b)
   (cond [(and (procedure? a) (procedure? b)) #t]
         [(and (pair? a) (pair? b)) (and (value-equal? (car a) (car b))
                                         (value-equal? (cdr a) (cdr b)))]
         [else (equal? a b)]))

 (define tests-failed '())

 (define (run-tests transformer evaluator tests)
   (set! tests-failed '())
   (for-each (curry run-test transformer evaluator)
             tests)
   (if (null? tests-failed)
       (display "\nall tests PASSED\n")
       (begin
         (display "\nFAILED tests:\n")
         (pretty-print tests-failed))))

 (define (run-test transformer evaluator expr)
   (let* ([test-e (transformer expr)]
          [test-res (evaluator expr)]
          [res (eval expr (environment '(rnrs)))]
          [test-passed (value-equal? test-res res)])
     (pretty-print expr)
     (for-each display (list "test result:  " test-res "\n"
                             "base result: " res "\n"
                             (if test-passed "test passed" "test FAILED")
                             "\n"))
     (when (not test-passed)
           (pretty-print test-e)
           (set! tests-failed (pair expr tests-failed)))
     (display "\n")))

 )