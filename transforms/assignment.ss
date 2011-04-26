#!r6rs

;; Assignment Conversion

;; based on "Design concepts in programming languages"
;; by FA Turbak, DK Gifford, MA Sheldon

;; input language:
;; tag | top-level-begin-define | self-eval | primitive | lambda | if | (A B) | begin | set!

;; output language:
;; tag | top-level-begin-define | self-eval | primitive | lambda | begin | if | (A B)

(library

 (transforms assignment)

 (export assignment-transform)

 (import (rnrs)
         (scheme-tools srfi-compat :1)
         (transforms common)
         (transforms syntax)
         (transforms utils))

 (define primitives (make-parameter '()))

 (define (primitive? var)
   (memq var (primitives))) 
 
 (define (mutated-free-vars e)
   (cond [(primitive? e) '()]
         [(tag? e) (mutated-free-vars (tag->expr e))]
         [(self-evaluating? e) '()]
         [(set? e) (pair (set->var e)
                         (mutated-free-vars (set->val e)))]
         [(lambda? e) (except (mutated-free-vars (lambda->body e))
                         (listify (lambda->args e)))]
         [(or (begin? e) (if? e) (application? e))
          (set-join (map mutated-free-vars (subexps e)))]
         [else (error e "mutated-free-vars: can't handle expr type")]))

 (define (mutated-free-vars* es)
   (set-join (map mutated-free-vars es)))

 (define (wrap-cells vars e)
   (if (null? vars)
       e
       `((lambda ,vars ,e)
         ,@(map (lambda (var) `(make-cell ,var)) vars))))

 ;; partition vars into two sets:
 ;; - ism: are assigned somewhere inside the given exprs
 ;; - isu: not assigned in any of the exprs
 (define (partition-vars vars es)
   (let ([e-vars (mutated-free-vars* es)])
     (values (both vars e-vars)
             (except vars e-vars))))

 (define (maybe-cell var vars e)
   (if (contains vars var)
       `(make-cell ,e)
       e))

 ;; vars, e -> e
 (define (amt vars e)
   ((amt-transformer e) vars e))

 ;; e -> (vars, e -> e)
 (define (amt-transformer e)
   (cond [(tag? e) amt-tag]
         [(symbol? e) amt-ref]
         [(self-evaluating? e) amt-self-evaluating]
         [(lambda? e) amt-lambda]
         [(set? e) amt-set]
         [(or (begin? e) (if? e)) amt-subexprs]
         [(application? e) amt-application]
         [else (error e "unknown expr type")]))

 (define (amt-tag vars e)
   (make-tag (amt vars (tag->expr e))
             (tag->name e)))

 (define (amt-ref vars e)
   (if (contains vars e)
       `(cell-ref ,e)
       e))

 (define (amt-self-evaluating vars e)
   e)

 (define (amt-lambda vars e)
   (let ([formals (listify (lambda->args e))])
     (let-values ([(ism isu) (partition-vars formals (list (lambda->body e)))])
       `(lambda ,(lambda->args e)
          ,(wrap-cells ism (amt (except (union (list vars ism) equal?) isu)
                                (lambda->body e)))))))

 (define (amt-set vars e)
   (if (primitive? (set->var e))
       (error e "cannot overwrite primitive!")
       `(set-cell! ,(set->var e)
                   ,(amt vars (set->val e)))))

 (define (amt-subexprs vars e)
   (mapsub (curry amt vars) e))
 
 (define (amt-application vars e)
   (map (curry amt vars) e))

 (define amt-primitives
   `((define make-cell (lambda (v) (cons 'cell v)))
     (define set-cell! (lambda (c v) (set-cdr! c v)))
     (define cell-ref (lambda (c) (cdr c)))))

 (define (top-amt e)
   ((begin-define-transform
     (lambda (def) (mapsub (lambda (e) (amt '() e)) def))
     (lambda (e) (amt '() e))) e))

 (define (assignment-transform e)
   (parameterize ([primitives (get-primitives e)])
                 (add-defines (top-amt e)
                              amt-primitives)))

 )