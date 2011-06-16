#!r6rs

(library

 (transform-tests common)

 (export common-tests
         begin-tests
         set-tests
         letrec-tests
         apply-tests)

 (import (rnrs))

 ;; language: common
 ;; self-eval | primitive | lambda | if | (A B)
 (define common-tests
   '(100
     'foo
     #t
     (cons 1 2)
     (not #t)
     (tag (not #t) 'tag1)
     ((lambda (x) x) 1)
     ((lambda (y) ((lambda (f) (f 3)) (if #f (lambda (x) x) (lambda (x) y)))) 2)
     ((lambda (x) x) 'foo)
     (if (> 1 0) #t #f)
     (lambda (f) (f 1 2))
     ((lambda (f) (f 1 2)) cons)
     ((lambda (f) (f 1 2 3)) (lambda (x y z) (list z y x)))
     ((lambda (g) (g ((lambda (f) (f 'ok 2)) g) 2)) cons)
     (if #t (display "ok") (void))
     (if #t (tag (display "ok") 'tag2) (void))
     (if #f (display "ok") (void))
     (if #f (tag (display "ok") 'tag3) (void))
     ))

 ;; language: common + apply
 (define apply-tests
   '(
     (apply (lambda (x) x) (list 1))
     (apply cons (list 1 2))
     (apply cons 1 (list 2))
     ((lambda (f) ((lambda (g) (apply g (list f #t))) (lambda (x y) (x y y)))) cons)
     ((lambda (f) ((lambda (g) (apply g f (list #t))) (lambda (x y) (x y y)))) cons)
     ))
 
 ;; language: common + begin
 (define begin-tests
   '(
     (begin 1 2 3)
     (begin
       ((lambda () 1)))
     (begin
       (define foo (lambda () 1))
       (foo))
     (begin
       (define foo (lambda (n) (if (= n 0) 'ok (foo (- n 1)))))
       (foo 5))
     (begin
       (define foo (lambda (n) (if (= n 0) 'ok (tag (foo (tag (- n 1) 'bar)) 'foo))))
       (foo 5))
     (begin
       (define foo (lambda (n) (if (= n 0) 'ok-foo (bar (- n 1)))))
       (define bar (lambda (n) (if (= n 0) 'ok-bar (foo (- n 1)))))
       (list (foo 5) (foo 6)))
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
       (list a ((car b)) b ((car a))))
     (letrec ([a 1])
       (letrec ([b a])
         (list a b)))
     ))

 )

