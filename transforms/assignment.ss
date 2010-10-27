#!r6rs

;; Assignment Conversion

;; based on "Design concepts in programming languages"
;; by FA Turbak, DK Gifford, MA Sheldon

;; input language:
;; self-eval | primitive | lambda | begin | if | set! | (A B)

;; output language:
;; self-eval | primitive | lambda | begin | if | (A B)

(library

 (transforms assignment)

 (export assignment-transform)

 (import (rnrs)
         (_srfi :1) ; lists
         (transforms syntax)
         (transforms utils))

 (define (mutated-free-vars e)
   (cond [(primitive? e) '()]
         [(self-evaluating? e) '()]
         [(set? e) (pair (set->var e)
                         (mutated-free-vars (set->val e)))]
         [(lambda? e) (except (mutated-free-vars (lambda->body e))
                         (listify (lambda->args e)))]
         [(or (begin? e) (if? e))
          (set-join (map mutated-free-vars (rest e)))]
         [(application? e)
          (set-join (map mutated-free-vars e))]
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
   (cond [(symbol? e) amt-ref]
         [(self-evaluating? e) amt-self-evaluating]
         [(lambda? e) amt-lambda]
         [(set? e) amt-set]
         [(or (begin? e) (if? e)) amt-subexprs]
         [(application? e) amt-application]
         [else (error e "unknown expr type")]))

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
          ,(wrap-cells ism (amt (except (union vars ism) isu)
                                (lambda->body e)))))))

 (define (amt-set vars e)
   (if (primitive? (set->var e))
       (error e "cannot overwrite primitive!")
       `(set-cell! ,(set->var e)
                   ,(amt vars (set->val e)))))

 (define (amt-subexprs vars e)
   `(,(first e)
     ,@(map (curry amt vars) (rest e))))

 (define (amt-application vars e)
   (map (curry amt vars) e))

 (define (with-amt-primitives e)
   `((lambda (make-cell set-cell! cell-ref)
       ,e)
     (lambda (v) (cons 'cell v))
     (lambda (c v) (set-cdr! c v))
     (lambda (c) (cdr c))))

 (define (assignment-transform e)
   (parameterize ([primitives (get-primitives e)])   
                 (with-amt-primitives (amt '() e))))

 )