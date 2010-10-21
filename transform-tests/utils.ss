#!r6rs

(library

 (transform-tests utils)

 (export value-equal?)

 (import (rnrs)
         (transforms utils))

 (define (value-equal? a b)
   (cond [(and (procedure? a) (procedure? b)) #t]
         [(and (pair? a) (pair? b)) (and (value-equal? (car a) (car b))
                                         (value-equal? (cdr a) (cdr b)))]
         [else (equal? a b)]))

 )