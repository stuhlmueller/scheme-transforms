#!r6rs

;; based on
;; http://github.com/darius/selfcentered

;; all lambdas take additional argument, continuation;
;; instead of returning, call continuation on result
;; every argument is either variable or lambda expression

;; application evaluates arguments one-by-one, constructing
;; continuation procedure that takes argument, either evaluates next
;; one or applies function 

(library

 (transforms cps)

 (export cps-transform
         cps-eval)

 (import (rnrs)
         (_srfi :1) ; lists
         (transforms syntax)
         (transforms utils)
         (only (ikarus) eval environment parameterize make-parameter pretty-print))

 (define primitives (make-parameter '()))

 (define (primitive? var)
   (memq var (primitives)))

 (define (cps-rename s)
   (string->symbol (string-append "cps-" (symbol->string s))))

 ;; this is for compilation to scheme with letrec
 (define (with-cps-primitives e ps)
   `(letrec ([cps-prim (lambda (f)
                         (lambda (k . args)
                           (k (apply f args))))]
             ,@(map (lambda (p) `(,(cps-rename p) (cps-prim ,p)))
                   ps))
      ,e))

 (define (cps e k)
   ((cps-converter e) e k))

 (define (cps* es k)
   (if (null? es)
       (k '())
       (let ([k1 (ngensym 'k*)])
         (cps (first es)
             `(lambda (,k1) ,(cps* (rest es)
                              (lambda (xs) (k (cons k1 xs)))))))))

 (define (cps-converter e)
   (cond [(primitive? e) cps-primitive]
         [(self-evaluating? e) cps-self-eval]
         [(lambda? e) cps-lambda]
         [(letrec? e) cps-letrec]
         [(begin? e) cps-begin]
         [(if? e) cps-if]
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

;; this version of cps-letrec makes use of letrec in the underlying scheme
(define (cps-letrec e k)
  `(letrec ,(map (lambda (def)
                   (let ([f (first def)]
                         [val (second def)])
                     (cond [(lambda? val)
                            (let ([vs (second val)]
                                  [fbody (third val)]
                                  [k1 (ngensym 'klr)])
                              `(,f (lambda (,k1 . ,vs)
                                     ,(cps fbody k1))))]
                           [(symbol? val)
                            `(,f ,val)]
                           ;; generates func that takes care of cont; but takes args
                           ;; that need to be evaluated before m-s-xrp is called
                           ;; [else (error val "cps-letrec: cannot handle val")]
                           [else
                            (begin
                              (when (not (tagged-list? val 'church-make-stateless-xrp))
                                    (for-each display (list "WARNING: potential non-proc in letrec -- " val "\n")))
                              (let ([k1 (ngensym 'klr2)])
                                `(,f (call/cc
                                      (lambda (,k1)
                                        ,(cps val k1))))))]
                           )))
                 (letrec->defns e))
     ,(cps (letrec->body e) k)))

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

;; higher-order functions need to pass on continuations correctly
(define no-override-primitives
  (list 'church-apply))
 
(define (cps-transform sexpr)
  ;; (pretty-print sexpr)
  (parameterize ([primitives (lset-difference equal?
                                              (get-primitives sexpr)
                                              no-override-primitives)])
                (begin
                  ;; (for-each display (list "primitives: " (primitives) "\n"))
                  (with-cps-primitives
                   (cps sexpr (let ([v (ngensym 'v)])
                                `(lambda (,v) ,v)))
                   (primitives)))))
 
 (define (cps-eval sexpr)
   (eval (cps-transform sexpr)
         (environment '(rnrs))))

 )