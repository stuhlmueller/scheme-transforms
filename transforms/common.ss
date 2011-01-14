#!r6rs

;; Letrec to Set

(library

 (transforms common)

 (export begin-define-transform)

 (import (rnrs)
         (scheme-tools srfi-compat :1)
         (transforms syntax)
         (transforms utils))

 (define (begin-define-transform def-transformer body-transformer)
   (lambda (e)
     (if (begin? e)
         (let* ([defs (begin->defs e)]
                [nondefs (begin->nondefs e)])
           `(begin
              ,@(map def-transformer defs)
              ,(body-transformer (begin-wrap nondefs))))
         (body-transformer e))))

 )