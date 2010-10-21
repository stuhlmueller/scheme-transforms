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
    ;; ((set? expr) close-set)
    ((letrec? expr) close-letrec)
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

 ;; this version does not support recursion
 ;; (define (close-letrec expr bound? free)
 ;;   (let* ([proc-names (map first (letrec->defns expr))]
 ;;          [letrec-bound? (lambda (v) (or ((bound-predicate proc-names) v)
 ;;                                    (bound? v)))])
 ;;     `(letrec ,(map (lambda (def) (list (first def)
 ;;                                   (convert-closure (second def) letrec-bound? free)))
 ;;                    (letrec->defns expr))
 ;;        ,(convert-closure
 ;;          (letrec->body expr)
 ;;          letrec-bound?
 ;;          free))))
 
 ;; FIXME: each proc-vectors only needs some free vars, some procs
 ;; how to allow each proc to refer to other procs explicitly?
 ;; - can't use letrec from underlying scheme, since vars in vectors
 ;;   need to refer to other vectors, but letrec only allows recursion for functions
 ;; - can't use (define x '#1=#((lambda () 'a) #1# 1)) since this will quote the whole object
 ;; - can't do self-reference using set! since generated code should be functional
 ;; - could do a y*-combinator transform, but this is probably inefficient
 ;; (define (close-letrec expr bound? free)
 ;;   (let* ([free-in-expr (get-free-vars expr (primitives))]
 ;;          [proc-names ...]
 ;;          [self1 (ngensym 'self)]
 ;;          [self2 (ngensym 'self)]
 ;;          [lr-self-refs (self-refs self2 (append proc-names free-in-expr))]
 ;;          [free-var-vals (map (lambda (name) (closure-env-ref name bound? free)) free-in-expr)]
 ;;          [proc-vectors (map (lambda (ld)
 ;;                               `(vector
 ;;                                 (lambda (,self ,@formals)
 ;;                                   ,(convert-closure
 ;;                                     (lambda->body expr)
 ;;                                     (bound-predicate formals)
 ;;                                     (self-refs self free-in-expr)))
 ;;                                 ,@PROCS
 ;;                                 ,@free-var-vals))
 ;;                             (map second (letrec->defns expr)))])
     ;; `((lambda (,self1) ((vector-ref ,self1 0) ,self1))
     ;;   (vector
     ;;    (lambda (,self2)
     ;;      ,(convert-closure (letrec->body expr)
     ;;                        never?
     ;;                        lr-self-refs))
     ;;    ,@proc-vectors
     ;;    ,@free-var-vals))))

 ;; can use (close-lambda expr bound? free)
 ;; if free vars are extended with proc names and dummy bindings
 ;; FIXME: does not need to reference all dummy bindings, only the required ones
 (define (bindings->vectors bindings bound? free)
   (let ([dummy-bindings (map (lambda (binding) `(,(def->name binding) . ',(def->name binding)))
                              bindings)])
     (map (lambda (binding)
            (close-lambda (def->lambda binding) bound? (append dummy-bindings free)))
          bindings)))

 (define (vector-expr-index v i)
   (let ([i (list-index (lambda (x) (equal? x i)) v)])
     (if (equal? i #f)
         #f
         (- i 1))))

 (define (vectors->refs names vectors)
   (apply append
          (map (lambda (subj-name subj-vec)
                 (filter-map
                  (lambda (obj-name obj-vec)
                    (let ([index (vector-expr-index subj-vec `',obj-name)])
                      (if (equal? index #f)
                          #f
                          `(vector-set! ,subj-name ,index ,obj-name))))
                  names vectors))
               names vectors)))
 
 ;; compute for each expression a vector with code, names of free vars + procs;
 ;; initial-vectors: vector definitions without recursive references go here, only placeholders
 ;; vector-references: of the form (vector-set! subj-vector subj-index obj-vector)
 ;;  
 ;; (lambda (A)
 ;;   (letrec ([foo (lambda () (list bar))]
 ;;            [bar (lambda () (list foo))])
 ;;     (foo A)))
 ;; =>
 ;; ((lambda (body-vector foo-vector bar-vector)
 ;;   (begin
 ;;     (vector-set! body-vector 1 foo-vector)
 ;;     (vector-set! body-vector 2 bar-vector)
 ;;     (vector-set! foo-vector 1 bar-vector)
 ;;     (vector-set! bar-vector 1 foo-vector)
 ;;     ((vector-ref body-vector 0) body-vector)))
 ;; (vector (lambda (s1) (list (vector-ref s1 1))) 'foo-vector 'bar-vector)
 ;; (vector (lambda (s2) (list (vector-ref s2 1))) 'bar-vector)
 ;; (vector (lambda (s3) (list (vector-ref s3 1))) 'foo-vector))
 (define (close-letrec expr bound? free)
   (let* ([proc-names (map first (letrec->defns expr))]
          [body-fv-names (get-free-vars (letrec->body expr) (append proc-names (primitives)))]
          [body-fv-vals (map (lambda (name) (closure-env-ref name bound? free)) body-fv-names)]
          [body-vector-name (ngensym 'body-vector)]
          [body-vector `(vector (lambda (,body-vector-name)
                                  ,(convert-closure
                                    (letrec->body expr)
                                    never?
                                    (self-refs body-vector-name (append proc-names body-fv-names))))
                                ,@(map (lambda (n) `',n) proc-names) ;; temp
                                ,@body-fv-vals)]
          [initial-vectors (bindings->vectors (letrec->defns expr) bound? free)]
          [vector-references (vectors->refs (pair body-vector-name proc-names)
                                            (pair body-vector initial-vectors))])
     `((lambda (,body-vector-name ,@proc-names)
         (begin
           ,@vector-references
           ((vector-ref ,body-vector-name 0) ,body-vector-name)))
       ,body-vector
       ,@initial-vectors)))
   
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
 
 (define (cc-transform e . verbose)
   (when (not (null? verbose))
         (pretty-print e))
   (parameterize ([primitives (get-primitives e)])
                 (cc e '())))

 (define (cc-eval e)
   (eval (cc-transform e)
         (environment '(rnrs))))

 )