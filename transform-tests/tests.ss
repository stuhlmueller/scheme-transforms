#!r6rs

(library

 (transform-tests tests)

 (export common-test-exprs)

 (import (rnrs))
 
 (define common-test-exprs
   '(100
     ((lambda (x) x) 'foo)
     (if (> 1 0) #t #f)
     (cons 1 2)
     (lambda (f) (f 1 2))
     ((lambda (f) (f 1 2)) cons)
     ((lambda (f) (f 1 2 3)) (lambda (x y z) (list z y x)))))

 )

