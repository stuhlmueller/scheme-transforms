#!r6rs

;; Continuation-Passing Style

;; based on http://github.com/darius/selfcentered

;; input language:
;; self-eval | primitive | lambda | begin | if | (A B)

;; output language:
;; self-eval | primitive | lambda | if | (A B)

(library

 (transforms cps)

 (export cps-transform)

 (import (rnrs)
         (_srfi :1) ; lists
         (transforms syntax)
         (transforms utils))

 (define (cps-rename s)
   (string->symbol (string-append "cps-" (symbol->string s))))

 (define (with-cps-primitives e ps)
   `((lambda (cps-prim)
       ((lambda ,(map cps-rename ps)
          ,e)
        ,@(map (lambda (p) `(cps-prim ,p)) ps)))
     (lambda (f)
       (lambda (k . args)
         (k (apply f args))))))

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
  (let ([k1 (ngensym 'kl)])
    `(,k (lambda (,k1 . ,(lambda->args e))
           ,(cps (lambda->body e) k1)))))

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

(define (cps-transform sexpr . args)
  (let ([bound-vars (if (null? args) '() (first args))]
        [cont-primitives (if (or (null? args) (null? (rest args)))
                             '()
                             (second args))])
    (parameterize
     ([primitives (except (get-primitives sexpr)
                          (append cont-primitives
                                  bound-vars))])
     (with-cps-primitives
      (cps sexpr (let ([v (ngensym 'v)])
                   `(lambda (,v) ,v)))
      (primitives)))))

 )