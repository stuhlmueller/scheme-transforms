#!r6rs

;; Closure Conversion

;; based on goo.gl/HdsQ and
;; "Design concepts in programming languages"
;; by FA Turbak, DK Gifford, MA Sheldon

;; input language:
;; tag | top-level-begin-define | self-eval | primitive | lambda | if | (A B) | apply | let

;; output language:
;; tag | top-level-begin-define | self-eval | primitive | lambda | if | (A B) | apply | let

(library

 (transforms cc)

 (export cc-transform)

 (import (rnrs)
         (scheme-tools srfi-compat :1)
         (scheme-tools object-id)
         (transforms common)
         (transforms syntax)
         (transforms utils))

 (define primitives (make-parameter '()))

 (define (primitive? var)
   (memq var (primitives)))  

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
    [(begin? e) (error e "closure-converter: begins should have been turned into lambdas!")]
    [(set? e) (error e "closure-converter: set! should have been turned into set-cell!")]
    [(letrec? e) (error e "closure-converter: letrecs should have been desugared into set+lambda!")]
    [(eq? e 'tag) close-tag]
    [(eq? e 'apply) (lambda (e bound? free) 'apply)]
    [(primitive-apply? e) close-primitive-apply]
    [(apply? e) close-apply] ;; partial support
    [(lambda? e) close-lambda]
    [(if? e) close-if]
;;    [(lambda-let? e) close-lambda-let]
    [(let? e) close-let]
    [(primitive? e) close-primitive]
    [(self-evaluating? e) close-self-evaluating]    
    [(application? e)
     (let ([op (app->opt e)])
       (if (primitive? op)
           close-primitive-application
           close-application))]
    [else (error e "closure-converter: unknown expression type")]))

 (define (primitive-apply? e)
   (and (apply? e)
        (primitive? (apply->proc e))))

 (define (bound-predicate names)
   (let ([list-of-names (listify names)])
     (lambda (name)
       (memq name list-of-names))))

 (define (closure-env-bind name value env)
   (cons (cons name value) env))
 
 (define (closure-env-ref name bound? free)
   (cond [(primitive? name) name]
         [(bound? name) name]
         [(assq name free) => cdr]
         [else (error name "unbound identifier")]))

 (define (close-tag e bound? free)
   (make-tag (cc (tag->expr e) bound? free) (tag->name e)))

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

 (define (close-apply e bound? free)
   (let ([op (cc (apply->proc e) bound? free)]
         [opname (ngensym 'op)])
     `(let ([,opname ,op])
        (apply
         (vector-ref ,opname 0)
         ,opname 
         ,@(close-sequence (apply->args e) bound? free)))))

 ;; common after cps, since each primitive
 ;; is rewritten to ... (apply proc args) ...
 (define (close-primitive-apply e bound? free)
   (let ([op (apply->proc e)])
     (assert-with-info (symbol? op) op)
     `(apply ,op
             ,@(close-sequence (apply->args e) bound? free))))
 
 (define (close-application e bound? free)
   (let ([op (cc (app->opt e) bound? free)]
         [opname (ngensym 'op)])
     `(let ([,opname ,op])
        ((vector-ref ,opname 0)
         ,opname 
         ,@(close-sequence (app->ops e) bound? free)))))

 (define (make-vector-expr lambda-expr args)
   `(vector ,lambda-expr
            ',(object->id lambda-expr) ;; ',lambda-expr
            ,@args))

 ;; A LAMBDA is converted to a closure.  The body is scanned for free
 ;; identifiers that are bound into the closure along with the
 ;; procedure body.  The body is converted in the context of the
 ;; LAMBDA's formal parameters and the new closure.
 (define (close-lambda e bound? free)
   (let* ((self (ngensym 'self))
          (formals (lambda->args e))
          (free-in-expr (get-free-vars e (append (primitives) reserved-words))))
     (make-vector-expr
      `(lambda (,self . ,formals)
         ,(cc
           (lambda->body e)
           (bound-predicate formals)
           (self-refs self free-in-expr)))
      (map (lambda (name)
             (closure-env-ref name bound? free))
           free-in-expr))))

 ;; Extract code expr, bind to var, then construct vector from var
 ;; in order to maintain cps style. 
 ;; In BODY of (lambda (self a b c) (let ([d ...]) BODY)), we access a b c directly.
 ;; whereas in (let ([d ...]) (lambda (self a b c) BODY)) we don't.
 ;; (define (close-lambda-let e bound? free)
 ;;   (let* ([closure-expr (cc (def->val (first (let->bindings e))) bound? free)]
 ;;          [closure-name (def->name (first (let->bindings e)))]
 ;;          [proc-expr (list-ref closure-expr 1)]
 ;;          [make-closure-expr (lambda (proc-expr) (make-vector-expr proc-expr (cdddr closure-expr)))] ;; cddr
 ;;          [proc-name (ngensym 'p)])
 ;;     `(let ([,proc-name ,proc-expr])
 ;;        (let ([,closure-name ,(make-closure-expr proc-name)])
 ;;          ,(cc (let->body e)
 ;;               (p-or bound?
 ;;                     (bound-predicate closure-name))
 ;;               free)))))

 ;; FIXME: verify that this is correct (maintains cps in as far as necessary)
 (define (close-let e bound? free)
   (let ([bindings (let->bindings e)])
     `(let ,(map (lambda (def) `(,(def->name def) ,(cc (def->val def) bound? free)))
                  bindings)
        ,(cc (let->body e)
             (p-or bound?
                   (bound-predicate (map def->name bindings)))
             free))))

 (define (close-primitive e bound? free)
   (let* ((self (ngensym 'self))
          (args (ngensym 'args)))
     (make-vector-expr
      `(lambda (,self . ,args)
         (apply ,e ,args))
      '())))
 
 ;; self-sym is a symbol, e.g. self12
 ;; free is the list of variables occuring free in an expression
 ;; this generates an association list
 ;; ((a . (vector-ref self12 1)) (b . (vector-ref self12 2)) ...)
 ;; thus, we can access variables using the self variable:
 ;; (vector (lambda (self) ...) a b ...)
 (define (self-refs self-sym free)
   (let loop ((ii 2) (names free) (env '())) ;; 1
     (if (null? names)
         env
         (loop (+ ii 1)
               (cdr names)
               (closure-env-bind
                (car names)
                `(vector-ref ,self-sym ,ii)
                env)))))

 (define (def->reference-fixes def)
   (let ([name (definition->name def)]
         [refs (cdddr (definition->value def))]) ;; cddr
     (map (lambda (ref i)
            `(vector-set! ,name ,(+ i 2) ,ref)) ;; 1
          refs (iota (length refs)))))
 
 (define (fix-recursive-references e)
   (assert-with-info (begin? e) e)
   (let* ([defs (begin->defs e)]
          [vector-defs (filter (lambda (def) (tagged-list? (definition->value def) 'vector)) defs)]
          [nondefs (begin->nondefs e)])
     `(begin
        ,@defs
        ,@(apply append (map def->reference-fixes vector-defs))
        ,@nondefs)))

 ;; need to initialize to #f, generate list of vector-set!s to adjust
 ;; recursive references?
 (define (top-cc e bound-vars)
   (if (begin? e)
       (let* ([defs (begin->defs e)]
              [nondefs (begin->nondefs e)]
              [bound? (bound-predicate (append bound-vars
                                               (map definition->name defs)))]
              [transformer (begin-define-transform
                            (lambda (def)
                              (let ([e (definition->value def)]
                                    [n (definition->name def)])
                                `(define ,n               
                                   ,(cond [(lambda? e) (close-lambda e bound? '())]
                                          [(symbol? e) e]
                                          [else (cc e bound? '())]))))
                            (lambda (e) (cc e bound? '())))])
         (fix-recursive-references (transformer e)))
       (cc e (bound-predicate bound-vars) '())))

 (define reserved-words '(set! let apply))

 (define (cc-transform e . args)
   (let ([bound-vars (if (null? args) '() (first args))])
     (parameterize ([primitives (get-primitives e (append reserved-words bound-vars))])
                   (top-cc e bound-vars))))

 )