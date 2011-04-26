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
         assert-with-info
         format
         pe
         p-or)
 
 (import (rnrs)
         (scheme-tools)
         (scheme-tools srfi-compat :1))

 (define-syntax assert-with-info
   (syntax-rules ()
     ((_ v info) (begin (when (not v) (pretty-print info))
                        (assert v)))))

 (define (curry fun . const-args)
   (lambda args
     (apply fun (append const-args args))))

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

 (define (set-join lsts)
   (delete-duplicates (apply append lsts)))

 (define (p-or . preds)
   (lambda args
     (any (lambda (x) x) (map (lambda (pred) (apply pred args)) preds))))

 )
