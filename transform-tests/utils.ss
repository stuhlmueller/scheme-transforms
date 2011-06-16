#!r6rs

(library

 (transform-tests utils)

 (export run-tests
         base-check
         evaluate
         app-evaluate)

 (import (rnrs)
         (transforms utils)
         (transforms syntax)
         (scheme-tools)
         (scheme-tools srfi-compat :1))

 (define (base-check a b)
   (cond [(and (procedure? a) (procedure? b)) #t]
         [(and (pair? a) (pair? b)) (and (base-check (car a) (car b))
                                         (base-check (cdr a) (cdr b)))]
         [else (equal? a b)]))

 (define tests-failed '())

 (define (for-each/indexed proc lst)
   (let ([total (length lst)])
     (for-each (lambda (elt i) (proc elt total (+ i 1)))
               lst
               (iota total))))

 (define (run-tests transformer checker evaluator tests)
   (set! tests-failed '())
   (for-each/indexed (curry run-test transformer checker evaluator)
                     tests)
   (if (null? tests-failed)
       (display (format "\nAll ~s tests PASSED.\n" (length tests)))
       (begin
         (display (format "\n~s FAILED tests:\n" (length tests-failed)))
         (map pretty-print tests-failed)))
   (list (- (length tests) (length tests-failed))
         (length tests)))

 (define (top-level-defines->letrec e)
   (if (begin? e)
       `(letrec ,(map rest (begin->defs e))
          ,(begin-wrap (begin->nondefs e)))
       e))

 (define (evaluate expr)
   (let ([e (top-level-defines->letrec expr)])
     (eval e (environment '(rnrs)
                          '(rnrs mutable-pairs)
                          '(only (scheme-tools) repl pe get-counter)
                          '(only (scheme-tools external) void)
                          '(transform-tests test-imports)))))

 (define (app-evaluate expr)
   (evaluate `(begin
                (define app (lambda (v) v))
                ,expr)))

 (define (with-exception-info thunk test-expr expr)
   (with-exception-handler
    (lambda (exn)
      (pe "exception!\n\n")
      (pretty-print expr)      
      (pe "\n")
      (pretty-print test-expr)
      (pe "\n")
      (raise exn))
    thunk))
 
 (define (run-test transformer checker evaluator expr total index)
   (pe "Running test " index "/" total " ... ")
   (let* ([test-expr (transformer expr)]
          ;; [_ (pretty-print test-expr)]
          [res (with-exception-info (lambda () (evaluator expr)) test-expr expr)]
          [test-res (with-exception-info (lambda () (evaluator test-expr)) test-expr expr)]
          [test-passed (checker test-res res)])
     (for-each display (list (->string:n test-res 15) " == " (->string:n res 15) " => "
                             (if test-passed "passed." "FAILED.")))
     (when (not test-passed)
           (display "converted expr:\n")
           (pretty-print test-expr)
           (set! tests-failed (pair expr tests-failed)))
     (display "\n")))

 )