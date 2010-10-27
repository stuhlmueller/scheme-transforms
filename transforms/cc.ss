#!r6rs

;; Closure Conversion

;; based on goo.gl/HdsQ

;; input language:
;; self-eval | primitive | lambda | if | (A B)

;; output language:
;; self-eval | primitive | lambda | if | (A B)

(library

 (transforms cc)

 (export cc-transform)

 (import (rnrs)
         (_srfi :1) ; lists
         (transforms syntax)
         (transforms utils))

 ;; e, bound, free -> e
 ;; - symbols referring to bound variables are left unmodified; bound?
 ;;   is a predicate that checks whether a symbol is bound
 ;; - free is an alist associating with each symbol referring to a
 ;;   free variable a self-vector-ref expr
 (define (cc e bound? free)
   ((closure-converter e) e bound? free))

 ;; [e], bound, free -> [e]
 (define (close-sequence seq bound? free)
   (map (lambda (item)
          (cc item bound? free))
        seq))

 ;; e -> (e, bound, free -> e)
 (define (closure-converter e)
   (cond
    ((begin? e) (error e "begins should have been turned into lambdas!"))
    ((set? e) (error e "set! should have been turned into set-cell!"))
    ((letrec? e) (error e "letrecs should have been desugared into set+lambda!"))    
    ((primitive? e) close-primitive)
    ((self-evaluating? e) close-self-evaluating)
    ((lambda? e) close-lambda)    
    ((if? e) close-if)
    ((application? e)
     (let ((op (app->opt e)))
       (cond
        ((primitive? op) close-primitive-application) ;; don't need to convert primitive that is immediately applied
        ((lambda? op) close-lambda-application)
        (else close-application))))
    (else (error e "unknown e type"))))

 (define (make-proper-list s)
   (cond [(null? s) '()]
         [(symbol? s) (list s)]
         [(pair? s) (pair (first s) (make-proper-list (rest s)))]))

 (define (bound-predicate names)
   (let ([list-of-names (make-proper-list names)])
     (lambda (name)
       (memq name list-of-names))))

 (define (closure-env-bind name value env)
   (cons (cons name value) env))
 
 (define (closure-env-ref name bound? free)
   (cond ((primitive? name) name)
         ((bound? name) name)
         ((assq name free) => cdr)
         (else (error name "unbound identifier"))))

 ;; An unbound identifier is converted to a %CLOSURE-REF.  Other atoms
 ;; are converted to themselves.
 (define (close-self-evaluating e bound? free)
   (if (symbol? e)
       (closure-env-ref e bound? free)
       e))

 (define (close-if e bound? free)
   `(if ,@(close-sequence (rest e) bound? free)))

 ;; Only arguments are converted in a primitive procedure application.
 (define (close-primitive-application e bound? free)
   `(,(app->opt e)
     ,@(close-sequence (app->ops e) bound? free)))

 ;; LAMBDA applications are treated as binding forms (there's no
 ;; primitive LET).  Don't convert the LAMBDA to a closure.  Lift free
 ;; identifiers.  Convert the lambda body in the context of its lifted
 ;; formal parameters.  Convert the arguments in the current context.
 ;;
 ;; Note: Lifting makes the code verbose.  It might not be necessary.
 ;; Revisit this after making a code emitter.  Feeley uses a primitive
 ;; LET form.
 (define (close-lambda-application e bound? free)
   (let* ((op (app->opt e))
          (free-in-expr (get-free-vars op (primitives)))
          ;; Lifting is accomplished with these appends.
          (formals (append (lambda->args op) free-in-expr))
          (args (append (app->ops e) free-in-expr)))
     `((lambda ,formals
         ,(cc
           (lambda->body op)
           (bound-predicate formals)
           '()))
       ,@(close-sequence args bound? free))))

 ;; A normal application is converted by looking up the procedure body
 ;; in the operator's closure and applying it to the operator along
 ;; with the converted arguments.
 ;; FIXME: this duplicates op
 ;; (define (close-application e bound? free)
 ;;   (let ((op (cc (app->opt e) bound? free)))
 ;;     `((vector-ref ,op 0)
 ;;       ,op
 ;;       ,@(close-sequence (app->ops e) bound? free))))
 ;; can I do this?
 (define (close-application e bound? free)
   (let ((op (cc (app->opt e) bound? free))
         (opname (ngensym 'op)))
     `((lambda (,opname)
         ((vector-ref ,opname 0)
          ,opname
          ,@(close-sequence (app->ops e) bound? free))) ,op)))
 

 ;; A LAMBDA is converted to a closure.  The body is scanned for free
 ;; identifiers that are bound into the closure along with the
 ;; procedure body.  The body is converted in the context of the
 ;; LAMBDA's formal parameters and the new closure.
 (define (close-lambda e bound? free)
   (let* ((self (ngensym 'self))
          (formals (lambda->args e))
          (free-in-expr (get-free-vars e (primitives))))
     `(vector
       (lambda (,self ,@formals)
         ,(cc
           (lambda->body e)
           (bound-predicate formals)
           (self-refs self free-in-expr)))
       ,@(map (lambda (name)
                (closure-env-ref name bound? free))
              free-in-expr))))

 (define (close-primitive e bound? free)
   (let* ((self (ngensym 'self))
          (args (ngensym 'args)))
     `(vector
       (lambda (,self . ,args)
         (apply ,e ,args))
       '()
       '())))
 
 ;; self is a symbol, e.g. self12
 ;; free is the list of variables occuring free in an expression
 ;; this generates an association list
 ;; ((a . (vector-ref self12 1)) (b . (vector-ref self12 2)) ...)
 ;; thus, we can access variables using self variable:
 ;; (vector (lambda (self) ...) a b ...)
 (define (self-refs self free)
   (let loop ((ii 1) (names free) (env '()))
     (if (null? names)
         env
         (loop (+ ii 1)
               (cdr names)
               (closure-env-bind
                (car names)
                `(vector-ref ,self ,ii)
                env)))))
 
 (define (cc-transform e)
   (parameterize ([primitives (get-primitives e)])
                 (cc e never? '())))

 )