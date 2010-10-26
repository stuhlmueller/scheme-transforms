#!r6rs

(import (rnrs)
        (transforms cc)
        (transform-tests utils)
        (transform-tests tests))

(define cc-test-exprs
  '((lambda (x) (begin (set! x 6) x)) 5))

(run-tests cc-transform
           cc-eval
           (append common-test-exprs
                   cc-test-exprs))