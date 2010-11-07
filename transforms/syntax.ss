#!r6rs

(library

 (transforms syntax)

 (export lambda->args
         lambda->body
         letrec->defns
         letrec->body
         application?
         app->opt
         app->ops
         ngensym
         self-evaluating?
         lambda-application?
         get-primitives
         lambda-app->args
         lambda-app->body
         lambda-app->ops
         lambda?
         letrec?
         get-free-vars
         if->test
         if->cons
         if->alt
         if?
         begin?
         set?
         set->var
         set->val
         def->name
         def->val
         mem?
         lambda-parameters
         lambda-body
         quoted?
         definition?
         definition->name
         definition->value
         free-variables
         mapsub
         subexps
         apply?
         apply->proc
         apply->args
         add-defines
         begin->nondefs
         begin->defs
         begin-wrap
         church-make-stateless-xrp?)

 (import (rnrs)
         (_srfi :1) ; lists
         (transforms utils))
 
 (define (mem? sexpr) (tagged-list? sexpr 'mem))
 (define (lambda-parameters exp) (cadr exp))
 (define (lambda-body exp) (caddr exp))
 (define (quoted? exp) (tagged-list? exp 'quote))
 (define (definition? exp) (tagged-list? exp 'define))
 (define definition->name second)
 (define definition->value third)
 (define (lambda? e) (tagged-list? e 'lambda))
 (define lambda->args second)
 (define lambda->body third)
 (define (letrec? e) (tagged-list? e 'letrec))
 (define letrec->defns second)
 (define letrec->body third)
 (define def->name first)
 (define def->val second)
 (define application? pair?)
 (define app->opt first)
 (define app->ops rest)
 (define if->test second)
 (define if->cons third)
 (define if->alt fourth)
 (define (apply? e) (tagged-list? e 'apply))
 (define apply->proc second)
 (define apply->args cddr)

 (define (begin? e) (tagged-list? e 'begin))
 (define (if? e) (tagged-list? e 'if))
 (define (set? e) (tagged-list? e 'set!))

 (define set->var second)
 (define set->val third)
 
 (define (lambda-application? e)
   (and (application? e)
        (tagged-list? (first e) 'lambda)))
 (define lambda-app->args cadar)
 (define lambda-app->body caddar)
 (define lambda-app->ops cdr)

 (define (church-make-stateless-xrp? e)
   (tagged-list? e 'church-make-stateless-xrp))

 (define symbol-index 0)
 (define (ngensym c)
   (set! symbol-index (+ 1 symbol-index))
   (string->symbol (string-append (symbol->string c)
                                  (number->string symbol-index))))

 (define (self-evaluating? e)
   (or (symbol? e)
       (tagged-list? e 'quote)
       (number? e)
       (boolean? e)
       (string? e)))

 (define (mapsub f e)
   (cond [(definition? e) `(define ,(definition->name e) ,(f (definition->value e)))]
         [(or (if? e) (begin? e) (set? e)) `(,(first e) ,@(map f (rest e)))]
         [(application? e) (map f e)]
         [else (error e "mapsub: unknown expression type")]))

 (define (subexps e)
   (cond [(or (if? e) (begin? e) (set? e)) (rest e)]
         [(application? e) e]
         [else (error e "subexps: unknown expression type")]))
   

 ;;this is used to find the free variables in a program, which need to
 ;;be provided by the header.
 (define (free-variables sexpr bound-vars)
   (cond
    ((begin? sexpr)
     (let ([new-bound (append (map definition->name (begin->defs sexpr))
                              bound-vars)])
       (apply append (map (lambda (e) (free-variables e new-bound)) (rest sexpr)))))
    ((definition? sexpr)
     (free-variables (definition->value sexpr) bound-vars))
    ((letrec? sexpr)
     (let ((new-bound (append (map first (second sexpr)) bound-vars)))
       (apply append (map (lambda (e) (free-variables e new-bound)) (pair (third sexpr) (map second (second sexpr)))))))
    ((quoted? sexpr) '())
    ((lambda? sexpr) (free-variables (lambda-body sexpr) (let loop ((params (lambda-parameters sexpr)))
                                                           (if (null? params)
                                                               bound-vars
                                                               (if (pair? params)
                                                                   (pair (first params) (loop (rest params)))
                                                                   (pair params bound-vars))))))
    ((if? sexpr)  (apply append (map (lambda (e) (free-variables e bound-vars)) (rest sexpr))))
    ((application? sexpr) (apply append (map (lambda (e) (free-variables e bound-vars)) sexpr)))
    ((symbol? sexpr) (if (memq sexpr bound-vars) '() (list sexpr)))
    (else '()) ))

 (define (get-free-vars sexpr . bound-vars)
   (delete-duplicates (free-variables sexpr (if (null? bound-vars) '() (first bound-vars)))))

 ;; FIXME
 (define get-primitives get-free-vars)

 (define (begin->defs e)
   (filter definition? (cdr e)))

 (define (begin->nondefs e)
   (filter (lambda (ei) (not (definition? ei))) (cdr e)))

 (define (add-defines e defs)
   (if (begin? e)
       `(begin
          ,@(begin->defs e)
          ,@defs
          ,@(begin->nondefs e))
       `(begin
          ,@defs
          ,e)))

 (define (begin-wrap exprs)
   (if (null? (rest exprs))
       (first exprs)
       `(begin ,@exprs)))
 
 )