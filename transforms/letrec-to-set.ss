#!r6rs

;; Letrec to Set

;; input language:
;; tag | top-level-begin-define | self-eval | primitive | lambda | if | (A B) | begin | set! | letrec

;; output language:
;; tag | top-level-begin-define | self-eval | primitive | lambda | if | (A B) | begin | set!

(library

 (transforms letrec-to-set)

 (export letrec-to-set)

 (import (rnrs)
         (scheme-tools srfi-compat :1)
         (transforms syntax)
         (transforms utils)
         (transforms common))

 (define primitives (make-parameter '()))

 (define (primitive? var)
   (memq var (primitives)))  

 (define (lrs e)
   (cond
    [(definition? e) (error e "letrec-to-set: cannot handle expr type")]
    [(tag? e) (make-tag (lrs (tag->expr e)) (tag->name e))]
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
    [(or (if? e) (begin? e) (set? e) (definition? e)) (mapsub lrs e)]
    [(application? e) (map lrs e)]
    [else (error e "letrec-to-set: cannot handle expr type")]))

 (define (top-lrs e)
   ((begin-define-transform
     (lambda (def) (mapsub lrs def))
     lrs) e))

 (define (letrec-to-set e)
   (parameterize ([primitives (get-primitives e)])
                 (top-lrs e)))

 )
