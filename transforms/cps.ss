#!r6rs

;; Efficient Continuation-Passing Style

;; based on "Design concepts in programming languages"
;; by FA Turbak, DK Gifford, MA Sheldon

;; input language:
;; tag | top-level-begin-define | self-eval | primitive | lambda | begin | if | (A B)

;; output language:
;; tag | top-level-begin-define | self-eval | primitive | lambda | if | (A B) | apply | let

(library

 (transforms cps)

 (export cps-transform)

 (import (rnrs)
         (scheme-tools srfi-compat :1)
         (transforms common)
         (transforms syntax)
         (transforms utils))

 (define primitives (make-parameter '()))

 (define (primitive? var)
   (memq var (primitives)))

 (define (cps-rename s)
   (string->symbol (string-append "cps-" (symbol->string s))))

 (define (mc->exp m)
   (let ([tmp_id (ngensym 'tmp)])
     `(lambda (,tmp_id) ,(m tmp_id))))
 
 (define (id->mc id)
   (lambda (v)
     `(,id ,v)))

 (define (meta-cps e)
   (cps e (lambda (x) x)))

 (define (cps e mc)
   ((cps-converter e) e mc))
 
 (define (cps-converter e)
   (cond [(letrec? e) (error e "letrec should have been desugared!")]
         [(set? e) (error e "set should have been desugared")]
         [(tag? e) cps-tag]
         [(primitive? e) cps-primitive]
         [(self-evaluating? e) cps-self-eval]
         [(lambda? e) cps-lambda]
         [(begin? e) cps-begin]
         [(if? e) cps-if]
         [(application? e)
          (if (primitive? (app->opt e))
              cps-primitive-application
              cps-application)]
         [else (error e "unknown expr type")]))

 (define (cps-tag e mc)
   (make-tag (cps (tag->expr e) mc) (tag->name e)))

 (define (cps-self-eval e mc)
   (mc e))

 ;; FIXME: rename e?
 (define (cps-primitive e mc)
   (mc (cps-rename e)))

 (define (cps-lambda e mc)
   (let ([id_abs (ngensym 'abs)]
         [id_k (ngensym 'k)]
         [args (lambda->args e)]
         [body (lambda->body e)])
     `(let ([,id_abs (lambda (,id_k . ,args) ,(cps body (id->mc id_k)))])
        ,(mc id_abs))))

 (define (cps-lambda/toplevel e mc)
   (let ([id_k (ngensym 'k)]
         [args (lambda->args e)]
         [body (lambda->body e)])
     (mc `(lambda (,id_k . ,args) ,(cps body (id->mc id_k))))))

 (define (cps-application e mc)
   (cps-application* e '() mc))
 
 ;; (define (cps-application* es vals mc)
 ;;   (if (null? es)
 ;;       (let* ([id_k (ngensym 'k)]
 ;;              [rvals (reverse vals)]
 ;;              [opt (first rvals)]
 ;;              [ops (drop rvals 1)])
 ;;         `(let ([,id_k ,(mc->exp mc)])
 ;;            (,opt ,id_k ,@ops)))
 ;;       (cps (first es)
 ;;            (lambda (v)
 ;;              (cps-application* (rest es) (pair v vals) mc)))))

 (define (cps-application* es vals mc)
   (if (null? es)
       (let* ([id_k (ngensym 'k)]
              [rvals (reverse vals)]
              [opt (first rvals)]
              [ops (drop rvals 1)])
         `(tag (,opt ,(mc->exp mc) ,@ops) app))
       (cps (first es)
            (lambda (v)
              (cps-application* (rest es) (pair v vals) mc)))))

 (define (cps-primitive-application e mc)
   (cps-primitive-application* (app->opt e)
                               (app->ops e)
                               '()
                               mc))

 ;; produces non-lambda let
 (define (cps-primitive-application* opt ops vals mc)
   (if (null? ops)
       (let ([id_ans (ngensym 'ans)])
         `(let ([,id_ans (,opt ,@(reverse vals))])
            ,(mc id_ans)))
       (cps (first ops)
            (lambda (v)
              (cps-primitive-application* opt (rest ops) (pair v vals) mc)))))

 ;; (define (cps-if e mc)
 ;;   (let ([test (if->test e)]
 ;;         [cons (if->cons e)]
 ;;         [alt (if->alt e)]
 ;;         [id_kif (ngensym 'kif)])
 ;;     (cps test
 ;;          (lambda (v)
 ;;            `(let ([,id_kif ,(mc->exp mc)])
 ;;               (if ,v
 ;;                   ,(cps cons (id->mc id_kif))
 ;;                   ,(cps alt (id->mc id_kif))))))))

 (define (cps-if e mc)
   (let ([test (if->test e)]
         [cons (if->cons e)]
         [alt (if->alt e)])
     (cps test
          (lambda (v)
            `(if ,v
                 ,(cps cons mc)
                 ,(cps alt mc))))))

 (define (cps-begin e mc)
   (cond [(null? (cdr e)) (mc '(void))] ;; (begin)
         [(null? (cddr e)) (cps (cadr e) mc)] ;; (begin E)
         [else (let ([kb (ngensym 'b)])      
                 (cps (second e)
                      (lambda (v)
                        `(let ([,kb ,v])
                           ,(cps `(begin ,@(cddr e)) mc)))))]))

 (define (top-cps e)
   ((begin-define-transform
     (lambda (def)
       (let ([e (definition->value def)]
             [n (definition->name def)])
         `(define ,n
            ,(cond [(church-make-stateless-xrp? e) (cps e (lambda (x) x))]
                   [(lambda? e) (cps-lambda/toplevel e (lambda (x) x))]
                   [(symbol? e) e]
                   [(self-evaluating? e) e]
                   [else (error e "top-cps: cannot handle expr")]))))
     (lambda (e)
       (meta-cps e)))
    e))

 (define (cps-primitives ps)
  `(,@(map (lambda (p) `(define ,(cps-rename p)
                     (lambda (k . args) (k (apply ,p args)))))
           ps)))

 (define reserved-words '(set! let apply abort tag))
 
 ;; meaning of primitives: globally free variables that are assumed to
 ;; be Scheme functions
 ;; - for primitives, cps-ified version is added (apply)
 ;; - for old defined lambdas, args are extended and body is cps-ified
 (define (cps-transform e . args)
   (let ([bound-vars (if (null? args) '() (first args))])
     (parameterize
      ([primitives (get-primitives e (append reserved-words bound-vars))])
      (add-defines (top-cps e)
                   (cps-primitives (primitives))
                   'top))))

 )