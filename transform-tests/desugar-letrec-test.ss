#!r6rs

;; forward-references in non-lambdas are not supported
;; (letrec ([baz (list foo)]
;;          [foo (lambda () (bar))]
;;          [bar (lambda () (list foo bar baz 1))])
;;   baz)

(import (rnrs)
        (_srfi :1)
        (transforms desugar-letrec)
        (transform-tests utils)
        (transform-tests tests))

(define
  letrec-tests
  '((letrec ([foo (list 1 2 3)]
             [bar (list 1 2 foo)])
      bar)
    (letrec ([make-plus (lambda (n) (lambda (x) (+ x n)))]
             [plus2 (make-plus 2)])
      (plus2 (plus2 3)))
    (letrec ([a (list (lambda () b))]
             [b (list (lambda () a))])
      (list a ((car b)) b ((car a))))
    (letrec ([baz (lambda () (list foo))]
             [foo (lambda () (bar))]
             [bar (lambda () (list foo bar baz 1))])
      (baz))
    ))

(run-tests desugar-letrec
           (lambda (e) (eval (desugar-letrec e) (environment '(rnrs))))
           letrec-tests)
