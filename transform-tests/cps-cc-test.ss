#!r6rs

(import (rnrs)
        (_srfi :1)
        (transforms cc)
        (transforms cps))

;; (define e '(((lambda (x) (lambda (y) x)) 2) 1))

;; (define e '(((lambda (x y) (lambda (z) (+ x y z 3))) 1 2) 3))

(define a '(letrec ([even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))]
                    [odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))])
             (even? 10)))

;; (define a '(letrec ([foo (lambda () (list foo))])
;;              (foo)))

;; (define a '(sqrt (((lambda (x) (lambda (y) x)) 2) 1)))
(define e (cps-transform a))

(display "\ncps-transform of ")
(display a)
(display "\n")
(pretty-print (cps-transform a))
(display "\n")

(display "\ncps-eval of ")
(display a)
(display "\n")
(display (cps-eval a))
(display "\n")

(display "\neval of ")
(display a)
(display "\n")
(display (eval a (environment '(rnrs))))
(display "\n")

(display "\ncc-transform of ")
(display e)
(display "\n")
(pretty-print (cc-transform e))
(display "\n")

(display "\ncc-eval of ")
(display e)
(display "\n")
(display (cc-eval e))
(display "\n")

(display "\neval of ")
(display e)
(display "\n")
(display (eval e (environment '(rnrs))))
(display "\n")