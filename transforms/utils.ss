#!r6rs

(library

 (transforms utils)
 
 (export pair
         rest
         repeat
         true
         false
         true?
         false?
         tagged-list?
         gensym
         curry
         compose
         except
         both
         union
         contains
         set-join
         listify
         eval
         environment
         parameterize
         make-parameter
         pretty-print
         assert
         format
         primitives
         primitive?
         pe
         never?)
 
 (import (_srfi :1)
         (rnrs)
         (only (ikarus)
               assert
               gensym
               eval
               environment
               parameterize
               make-parameter
               pretty-print
               format))

 (define (pe . args)
   (for-each display args))

 (define primitives (make-parameter '()))

 (define (primitive? var)
   (memq var (primitives))) 

 (define (curry fun . const-args)
   (lambda args
     (apply fun (append const-args args))))

 (define (compose . fns)
  (define (make-chain fn chain)
    (lambda args
      (call-with-values (lambda () (apply fn args)) chain)))
  (reduce make-chain values fns))

 (define rest cdr)
 
 (define pair cons)
 
 (define true #t)

 (define false #f)
 
 (define (true? x)
   (not (eq? x false)))
 
 (define (false? x)
   (eq? x false))
 
 (define (repeat n thunk)
   (if (> n 0)
       (pair (thunk) (repeat (- n 1) thunk))
       (list) ))
 
 (define (tagged-list? exp tag)
   (if (pair? exp)
       (eq? (car exp) tag)
       false))

 (define (listify p)
   (cond [(null? p) '()]
         [(symbol? p) (pair p '())]
         [(pair? p) (pair (first p) (listify (rest p)))]
         [else (error p "pairs->list: can't handle expr type")]))

 (define (except vals not-vals)
   (lset-difference eq? vals not-vals))

 (define (both vals-a vals-b)
   (lset-intersection eq? vals-a vals-b))

 (define (contains vars var)
   (not (null? (both vars (list var)))))

 (define (union vals-a vals-b)
   (lset-union eq? vals-a vals-b))

 (define (set-join lsts)
   (delete-duplicates (apply append lsts)))

 (define (never? _) #f)

 )
