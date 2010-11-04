#!r6rs

;; Letrec to Set

;; input language:
;; self-eval | primitive | lambda | if | (A B) | begin | set! | letrec

;; output language:
;; self-eval | primitive | lambda | if | (A B) | begin | set!

(library

 (transforms letrec-to-set)

 (export letrec-to-set)

 (import (rnrs)
         (_srfi :1) ; lists
         (transforms syntax)
         (transforms utils))

 (define (lrs e)
   (cond
    [(primitive? e) e]
    [(self-evaluating? e) e]
    [(lambda? e) `(lambda ,(lambda->args e) ,(lrs (lambda->body e)))]
    [(letrec? e)
     (let* ([defns (letrec->defns e)]
            [names (map def->name defns)]
            [vals (map def->val defns)])
       `((lambda ,names
           (begin
             ,@(map (lambda (n v) `(set! ,n ,(lrs v))) names vals)
             ,(lrs (letrec->body e))))
         ,@(make-list (length names) '#f)))]
    [(or (if? e) (begin? e) (set? e)) (mapsub lrs e)]
    [(application? e) (map lrs e)]
    [else (error e "letrec-to-set: cannot handle expr type")]))

 (define (letrec-to-set e)
   (parameterize ([primitives (get-primitives e)])
                 (lrs e)))
 

 )
