#!r6rs

(library

 (transforms syntax)

 (export add-defines
         app->ops
         app->opt
         application?
         apply->args
         apply->proc
         apply?
         begin->defs
         begin->nondefs
         begin-wrap
         begin?
         church-make-stateless-xrp?
         def->name
         def->val
         definition->name
         definition->value
         definition?
         form?
         free-variables
         get-free-vars
         get-primitives
         if->alt
         if->cons
         if->test
         if?
         lambda->args
         lambda->body
         lambda-app->args
         lambda-app->body
         lambda-app->ops
         lambda-application?
         lambda-body
         lambda-let?
         lambda-parameters
         lambda?
         let->bindings
         let->body
         let?
         letrec->body
         letrec->defns
         letrec?
         local
         make-tag
         mapsub
         mem?
         ngensym
         quoted?
         self-evaluating?
         set->val
         set->var
         set?
         subexps
         tag->expr
         tag->name
         tag?
         tagged?)

 (import (rnrs)
         (scheme-tools srfi-compat :1)
         (transforms utils))

 (define (tag? sexpr) (tagged-list? sexpr 'tag))
 (define (tagged? e name)
   (and (tag? e)
        (equal? (tag->name e) name)))
 (define tag->expr second)
 (define tag->name third)
 (define (make-tag expr name) (list 'tag expr name))
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
 (define (application? e)
   (and (list? e)
        (not (null? e))
        (not (form? e))))
 (define app->opt first)
 (define app->ops rest)
 (define if->test second)
 (define if->cons third)
 (define if->alt fourth)
 (define (apply? e) (tagged-list? e 'apply))
 (define apply->proc second)
 (define apply->args cddr)
 (define (let? e) (tagged-list? e 'let))
 (define let->bindings second)
 (define (let->body e) (begin (assert-with-info (= (length e) 3) e) (third e)))
 (define (lambda-let? e) (and (tagged-list? e 'let)
                         (= (length (let->bindings e)) 1)
                         (lambda? (def->val (first (let->bindings e))))))

 (define (begin? e) (tagged-list? e 'begin))
 (define (if? e) (tagged-list? e 'if))
 (define (set? e) (tagged-list? e 'set!))
 (define set->var second)
 (define set->val third)

 (define (form? e)
   (or (tag? e)
       (lambda? e)
       (quoted? e)
       (definition? e)
       (letrec? e)
       (if? e)
       (apply? e)
       (let? e)
       (begin? e)
       (set? e)))
 
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
     (let ((new-bound (append (map def->name (let->bindings sexpr)) bound-vars)))
       (apply append (map (lambda (e) (free-variables e new-bound)) (pair (third sexpr) (map second (second sexpr)))))))
    ((let? sexpr)
     (let ((new-bound (append (map def->name (let->bindings sexpr)) bound-vars)))
       (append (apply append (map (lambda (e) (free-variables e bound-vars)) (map def->val (let->bindings sexpr))))
               (free-variables (let->body sexpr) new-bound))))
    ((quoted? sexpr) '())
    ((lambda? sexpr) (free-variables (lambda-body sexpr) (let loop ((params (lambda-parameters sexpr)))
                                                           (if (null? params)
                                                               bound-vars
                                                               (if (pair? params)
                                                                   (pair (first params) (loop (rest params)))
                                                                   (pair params bound-vars))))))
    ((if? sexpr)  (apply append (map (lambda (e) (free-variables e bound-vars)) (rest sexpr))))
    
    (((p-or application? tag? apply? set?) sexpr) (apply append (map (lambda (e) (free-variables e bound-vars)) sexpr)))
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

 (define (add-defines e defs . top)
   (if (begin? e)
       (if (not (null? top))
           `(begin
              ,@defs              
              ,@(begin->defs e)
              ,@(begin->nondefs e))
           `(begin
              ,@(begin->defs e)
              ,@defs
              ,@(begin->nondefs e)))
       `(begin
          ,@defs
          ,e)))

 (define (begin-wrap exprs)
   (if (null? (rest exprs))
       (first exprs)
       `(begin ,@exprs)))

 (define (local expr)
   `((lambda () ,expr)))
 
 )