#!r6rs

(library

 (transform-tests tests)

 (export common-test-exprs
         cps-test-exprs
         cc-test-exprs)

 (import (rnrs))

 (define cps-test-exprs
   '((begin 1 2 3)))

 (define cc-test-exprs
   '())
 
 (define common-test-exprs
   '(100
     ((lambda (x) x) 'foo)
     (if (> 1 0) #t #f)
     (cons 1 2)
     (lambda (f) (f 1 2))
     ((lambda (f) (f 1 2)) cons)
     ((lambda (f) (f 1 2 3)) (lambda (x y z) (list z y x)))
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
       (list a ((car b)) b ((car a))))
     ))

 )

