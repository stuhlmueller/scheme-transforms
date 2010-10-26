#!r6rs

(import (rnrs)
        (_srfi :1)
        (transforms cps)
        (transform-tests utils)
        (transform-tests tests))

(define cps-test-exprs
   '((begin 1 2 3)
     (letrec ([my-even? (lambda (n) (if (= n 0) #t (my-odd? (- n 1))))]
              [my-odd? (lambda (n) (if (= n 0) #f (my-even? (- n 1))))])
       (my-even? 10))
     (letrec ([baz (lambda () foo)]
              [foo (lambda () (bar))]
              [bar (lambda () (list foo bar baz 1))])
       (baz))
     (letrec ([foo (list 1 2 3)]
              [bar (list 1 2 foo)])
       bar)
     (letrec ([make-plus (lambda (n) (lambda (x) (+ x n)))]
              [plus2 (make-plus 2)])
       (plus2 (plus2 3)))
     (letrec ([a (list (lambda () b))]
              [b (list (lambda () a))])
       (list a ((car b)) b ((car a))))))

(run-tests cps-transform
           cps-eval
           (append common-test-exprs
                   cps-test-exprs))