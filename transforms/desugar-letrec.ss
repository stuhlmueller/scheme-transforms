#!r6rs

(library

 (transforms desugar-letrec)

 (export desugar-letrec)

 (import (rnrs)
         (_srfi :1) ; lists
         (transforms syntax)
         (transforms utils))

 ;; not recursive!
 (define (desugar-letrec e)
   (assert (letrec? e))
   (let* ([defns (letrec->defns e)]
          [names (map def->name defns)]
          [vals (map def->val defns)])
     `((lambda ,names
           (begin
             ,@(map (lambda (n v) `(set! ,n ,v)) names vals)
             ,(letrec->body e)))
       ,@(make-list (length names) '#f))))

 )
