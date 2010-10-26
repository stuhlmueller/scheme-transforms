#!r6rs

;; based on 
;; http://github.com/weaver/sic/blob/93bb2d9eb3b528dbd5348da0d5f6496244e441cf/cps.scm

(library

 (transforms cc)

 (export cc-transform
         cc-eval)

 (import (rnrs)
         (_srfi :1) ; lists
         (transforms syntax)
         (transforms utils)         
         (only (ikarus) eval environment parameterize make-parameter pretty-print))

 (define primitives (make-parameter '()))

 (define (primitive? var)
   (memq var (primitives)))

 (define (never? _) #f)

 (define (cc expr env)
   (convert-closure expr never? env))

 ;; - symbols referring to bound variables are left unmodified; bound?
 ;;   is a predicate that checks whether a symbol is bound
 ;; - free is an alist associating with each symbol referring to a
 ;;   free variable a self-vector-ref expr
 (define (convert-closure expr bound? free)
   (let ((handler (closure-converter expr)))
     ;; (for-each display (list expr " -> " handler "\n"))
     (handler expr bound? free)))

 (define (close-sequence seq bound? free)
   (map (lambda (item)
          (convert-closure item bound? free))
        seq))

 (define (closure-converter expr)
   (cond
    ((self-evaluating? expr) close-self-evaluating)
    ((lambda? expr) close-lambda)
    ((if? expr) close-if)
    ((set? expr) close-set)
    ((letrec? expr) (error expr "letrecs should have been desugared into set+lambda!"))
    ((application? expr)
     (let ((op (app->opt expr)))
       (cond ((primitive? op) close-primitive-application)
             ((lambda? op) close-lambda-application)
             (else close-application))))
    (else (error expr "unknown expr type"))))

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
 (define (close-self-evaluating expr bound? free)
   (if (symbol? expr)
       (closure-env-ref expr bound? free)
       expr))

 (define (close-if expr bound? free)
   `(if ,@(close-sequence (rest expr) bound? free)))

 ;; Only arguments are converted in a primitive procedure application.
 (define (close-primitive-application expr bound? free)
   `(,(app->opt expr)
     ,@(close-sequence (app->ops expr) bound? free)))

 ;; LAMBDA applications are treated as binding forms (there's no
 ;; primitive LET).  Don't convert the LAMBDA to a closure.  Lift free
 ;; identifiers.  Convert the lambda body in the context of its lifted
 ;; formal parameters.  Convert the arguments in the current context.
 ;;
 ;; Note: Lifting makes the code verbose.  It might not be necessary.
 ;; Revisit this after making a code emitter.  Feeley uses a primitive
 ;; LET form.
 (define (close-lambda-application expr bound? free)
   (let* ((op (app->opt expr))
          (free-in-expr (get-free-vars op (primitives)))
          ;; Lifting is accomplished with these appends.
          (formals (append (lambda->args op) free-in-expr))
          (args (append (app->ops expr) free-in-expr)))
     `((lambda ,formals
         ,(convert-closure
            (lambda->body op)
            (bound-predicate formals)
            '()))
       ,@(close-sequence args bound? free))))

 ;; A normal application is converted by looking up the procedure body
 ;; in the operator's closure and applying it to the operator along
 ;; with the converted arguments.
 ;; FIXME: this duplicates op
 ;; (define (close-application expr bound? free)
 ;;   (let ((op (convert-closure (app->opt expr) bound? free)))
 ;;     `((vector-ref ,op 0)
 ;;       ,op
 ;;       ,@(close-sequence (app->ops expr) bound? free))))
 ;; can I do this?
 (define (close-application expr bound? free)
   (let ((op (convert-closure (app->opt expr) bound? free))
         (opname (ngensym 'op)))
     `((lambda (,opname)
         ((vector-ref ,opname 0)
          ,opname
          ,@(close-sequence (app->ops expr) bound? free))) ,op)))
    

 ;; A LAMBDA is converted to a closure.  The body is scanned for free
 ;; identifiers that are bound into the closure along with the
 ;; procedure body.  The body is converted in the context of the
 ;; LAMBDA's formal parameters and the new closure.
 (define (close-lambda expr bound? free)
   (let* ((self (ngensym 'self))
          (formals (lambda->args expr))
          (free-in-expr (get-free-vars expr (primitives))))
     `(vector
       (lambda (,self ,@formals)
         ,(convert-closure
            (lambda->body expr)
            (bound-predicate formals)
            (self-refs self free-in-expr)))
       ,@(map (lambda (name)
                (closure-env-ref name bound? free))
              free-in-expr))))
   
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

 (define (closure-env-set name val-expr bound? free)
  (cond ((primitive? name) (error name "cannot overwrite primitive name!"))
        ((bound? name) `(set! ,name ,val-expr))
        ((assq name free) => (lambda (b) `(vector-set! ,@(cddr b) ,val-expr))) ;; FIXME
        (else (error name "set: unbound identifier"))))

;; FIXME: what to do about this? need to set correct element in self vector
(define (close-set expr bound? free)
  (closure-env-set (set->var expr)
                   (convert-closure (set->val expr) bound? free)
                   bound?
                   free))
 
 (define (cc-transform e . verbose)
   (when (not (null? verbose))
         (pretty-print e))
   (parameterize ([primitives (get-primitives e)])
                 (cc e '())))

 (define (cc-eval e)
   (eval (cc-transform e)
         (environment '(rnrs))))

 )