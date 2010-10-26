#!r6rs

(import (rnrs)
        (transforms cc)
        (transform-tests utils)
        (transform-tests tests))

(define cc-test-exprs

  ;; set + lambda app
  '(

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

    )

  )

(define (cc-check a b)
   (cond [(and (vector? a) (procedure? b)) #t]
         [(and (pair? a) (pair? b)) (and (cc-check (car a) (car b))
                                         (cc-check (cdr a) (cdr b)))]
         [else (equal? a b)]))

(run-tests cc-transform
           cc-eval
           cc-check
           (append common-test-exprs
                   cc-test-exprs))