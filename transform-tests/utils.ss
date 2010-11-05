#!r6rs

(library

 (transform-tests utils)

 (export run-tests
         base-check)

 (import (rnrs)
         (transforms utils)
         (transforms syntax)
         (_srfi :1))

 (define (base-check a b)
   (cond [(and (procedure? a) (procedure? b)) #t]
         [(and (pair? a) (pair? b)) (and (base-check (car a) (car b))
                                         (base-check (cdr a) (cdr b)))]
         [else (equal? a b)]))

 (define tests-failed '())

 (define (run-tests transformer checker tests)
   (set! tests-failed '())
   (for-each (curry run-test transformer checker)
             tests)
   (if (null? tests-failed)
       (display (format "\nall ~s tests PASSED\n" (length tests)))
       (begin
         (display (format "\n~s FAILED tests:\n" (length tests-failed)))
         (map pretty-print tests-failed))))

 (define (top-level-defines->letrec e)
   (if (begin? e)
       `(letrec ,(map rest (begin->defs e))
          ,(begin-wrap (begin->nondefs e)))
       e))

 (define (evaluate expr)
   (let ([e (top-level-defines->letrec expr)])
     (eval e (environment '(rnrs) '(rnrs mutable-pairs)))))
 
 (define (run-test transformer checker expr)
   (display "Running Test:\n")
   ;; (pretty-print expr)
   (let* ([test-expr (transformer expr)]
          ;; [_ (pretty-print test-expr)]
          [test-res (evaluate test-expr)]
          [res (evaluate expr)]
          [test-passed (checker test-res res)])
     (for-each display (list "test result:  " test-res "\n"
                             "base result: " res "\n"
                             (if test-passed "test passed" "test FAILED")
                             "\n"))
     (when (not test-passed)
           (display "converted expr:\n")
           (pretty-print test-expr)
           (set! tests-failed (pair expr tests-failed)))
     (display "\n")))

 )