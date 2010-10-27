#!r6rs

(library

 (transform-tests common)

 (export common-tests
         begin-tests
         set-tests
         letrec-tests)

 (import (rnrs))

 ;; language: common
 ;; self-eval | primitive | lambda | if | (A B)
 (define common-tests
   '(100
     'foo
     #t
     (cons 1 2)
     (not #t)
     ((lambda (y) ((lambda (f) (f 3)) (if #f (lambda (x) x) (lambda (x) y)))) 2)
     ((lambda (x) x) 'foo)
     (if (> 1 0) #t #f)
     (lambda (f) (f 1 2))
     ((lambda (f) (f 1 2)) cons)
     ((lambda (f) (f 1 2 3)) (lambda (x y z) (list z y x)))))

 ;; language: common + begin
 (define begin-tests
   '(
     (begin 1 2 3)
     ))

 ;; language: common + begin + set
 (define set-tests
   '(
    ((lambda (x) (cons (set! x 3) x)) 5)

    ((lambda (f x) (f (set! x 3) x)) cons 5)

    ((lambda (x)
       ((lambda (k) (k (set! x 6)))
        (lambda (v) x))) 5)

    ((lambda (f) (f 5))
     (lambda (x)
       ((lambda (k) (k (set! x 6)))
        (lambda (v) x))))

    ((lambda (box) ((car box) 5))
     (cons (lambda (x)
             ((lambda (k) (k (set! x 6)))
              (lambda (v) x)))
           'foo))

    ;; this requires vector-set
    ((lambda (x)
       ((lambda (f) (f (lambda (v) x)))
        (lambda (k) (k (set! x 6))))) 5)     
     ))

 ;; language: common + begin + set + letrec
 (define letrec-tests
   '(
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

 )

