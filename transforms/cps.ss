#!r6rs

;; Continuation-Passing Style

;; based on http://github.com/darius/selfcentered

;; input language:
;; self-eval | primitive | lambda | begin | if | (A B) | top-level-begin-define

;; output language:
;; self-eval | primitive | lambda | if | (A B) | apply | top-level-begin-define

(library

 (transforms cps)

 (export cps-transform)

 (import (rnrs)
         (_srfi :1) ; lists
         (transforms syntax)
         (transforms utils))

 ;; e, k -> e
 (define (cps e k)
   ((cps-converter e) e k))

 ;; [e], k -> e
 (define (cps* es k)
   (if (null? es)
       (k '())
       (let ([k1 (ngensym 'k*)])
         (cps (first es)
             `(lambda (,k1) ,(cps* (rest es)
                              (lambda (xs) (k (cons k1 xs)))))))))

 ;; e -> (e, k -> e)
 (define (cps-converter e)
   (cond [(letrec? e) (error e "letrec should have been desugared!")]
         [(primitive? e) cps-primitive]
         [(self-evaluating? e) cps-self-eval]
         [(lambda? e) cps-lambda]
         [(begin? e) cps-begin]
         [(if? e) cps-if]
         [(set? e) cps-set]
         [(application? e)
          (if (primitive? (app->opt e))
              cps-primitive-application
              cps-application)]
         [else (error e "unknown expr type")]))

 (define (cps-primitive e k)
   `(,k ,(cps-rename e)))
 
 (define (cps-self-eval e k)
   `(,k ,e))

(define (cps-lambda e k)
  `(,k ,(cps-defined-lambda e)))

(define (cps-defined-lambda e)
  (let ([k1 (ngensym 'kl)])  
    `(lambda (,k1 . ,(lambda->args e))
       ,(cps (lambda->body e) k1))))

(define (cps-set e k)
  (let ([r (ngensym 'r)])
    (cps (set->val e)
         `(lambda (,r)
            (,k (set! ,(set->var e) ,r))))))

(define (cps-begin e k)
  (if (= (length e) 2)
      (cps (second e) k)
      (let ([k1 (ngensym 'kb)])      
        (cps (second e)
             `(lambda (,k1) ,(cps-begin `(begin . ,(cddr e)) k))))))

(define (cps-if e k)
  (let ([k1 (ngensym 'ki)]
        [ce (cps (if->cons e) k)]
        [ae (cps (if->alt e) k)])
    (cps (if->pred e)
         `(lambda (,k1)
            (if ,k1 ,ce ,ae)))))

(define (cps-primitive-application e k)
  (cps* (app->ops e)
        (lambda (xs) `(,(cps-rename (app->opt e)) ,k . ,xs))))

(define (cps-application e k)
  (cps* e (lambda (x) (let ([f (first x)]
                       [xs (rest x)])
                   `(,f ,k . ,xs)))))

(define (cps-rename s)
   (string->symbol (string-append "cps-" (symbol->string s))))

(define (cps-primitives ps)
  `(,@(map (lambda (p) `(define ,(cps-rename p)
                     (lambda (k . args) (k (apply ,p args)))))
           ps)))

(define (top-cps e)
  (let ([k (let ([v (ngensym 'v)])
             `(lambda (,v) ,v))])    
    (if (begin? e)
        (let* ([defs (begin->defs e)]
               [nondefs (begin->nondefs e)]
               [cps-e (cps (begin-wrap nondefs) k)])
          `(begin
             ,@(map (lambda (def)
                      (begin (assert (lambda? (definition->value def)))
                             `(define
                                ,(definition->name def)
                                ,(cps-defined-lambda (definition->value def)))))
                    defs)
             ,cps-e))
        (cps e k))))

;; meaning of primitives: globally free variables that are assumed to
;; be Scheme functions
;; - for primitives, cps-ified version is added (apply)
;; - for old defined lambdas, args are extended and body is cps-ified
(define (cps-transform e . args)
  (let ([bound-vars (if (null? args) '() (first args))])
    (parameterize
     ([primitives (get-primitives e bound-vars)])
     (add-defines
      (top-cps e)
      (cps-primitives (primitives))))))

 )