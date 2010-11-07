#!r6rs

;; Letrec to Set

(library

 (transforms common)

 (export begin-define-transform)

 (import (rnrs)
         (_srfi :1) ; lists
         (transforms syntax)
         (transforms utils))

 (define (begin-define-transform def-transformer body-transformer)
   (lambda (e)
     (if (begin? e)
         (let* ([defs (begin->defs e)]
                [nondefs (begin->nondefs e)])
           `(begin
              ,@(map (lambda (def)
                       (begin
                         (assert-with-info
                          ((p-or lambda? church-make-stateless-xrp? symbol?) (definition->value def))
                          def)
                         (def-transformer def)))
                     defs)
              ,(body-transformer (begin-wrap nondefs))))
         (body-transformer e))))

 )