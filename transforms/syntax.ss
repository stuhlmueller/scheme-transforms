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
         if->pred
         if->cons
         if->alt
         if?
         begin?
         set?
         set->var
         set->val
         def->name
         def->lambda
         mem?
         lambda-parameters
         lambda-body
         quoted?
         definition?
         free-variables)

 (import (rnrs)
         (_srfi :1) ; lists
         (transforms utils))

 (define (mem? sexpr) (tagged-list? sexpr 'mem))
 (define (lambda-parameters exp) (cadr exp))
 (define (lambda-body exp) (caddr exp))
 (define (quoted? exp) (tagged-list? exp 'quote))
 (define (definition? exp) (tagged-list? exp 'define))
 (define (lambda? e) (tagged-list? e 'lambda))
 (define lambda->args second)
 (define lambda->body third)
 (define (letrec? e) (tagged-list? e 'letrec))
 (define letrec->defns second)
 (define letrec->body third)
 (define def->name first)
 (define def->lambda second)
 (define application? pair?)
 (define app->opt first)
 (define app->ops rest)
 (define if->pred second)
 (define if->cons third)
 (define if->alt fourth)

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

 ;;this is used to find the free variables in a program, which need to be provided by the header. (will also be used by caching...)
 (define (free-variables sexpr bound-vars)
   (cond
    ((begin? sexpr) (apply append (map (lambda (e) (free-variables e bound-vars)) (rest sexpr))))
    ((letrec? sexpr)
     (let ((new-bound (append (map first (second sexpr)) bound-vars)))
       (apply append (map (lambda (e) (free-variables e new-bound)) (pair (third sexpr) (map second (second sexpr)))))))
    ;;((self-evaluating? sexpr) (make-syntax 'self-evaluating sugared-sexpr sexpr) )
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
 
 )