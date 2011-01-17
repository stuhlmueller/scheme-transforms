#!r6rs

;; App Conversion

;; Takes a program in continuation passing style and converts all
;; function applications into returns to the top-level that pass
;; function, continuation, and args back.

;; input language:
;; tag | top-level-begin-define | self-eval | primitive | lambda | if | (A B) | apply | let

;; output language:
;; tag | top-level-begin-define | self-eval | primitive | lambda | if | (A B) | apply | let

(library

 (transforms app)

 (export app-transform)

 (import (rnrs)
         (scheme-tools srfi-compat :1)
         (transforms common)
         (transforms syntax)
         (transforms utils))

 (define (app-transform e)
   (cond [(tagged? e 'app) (app-application e)]
         [(list? e) (map app-transform e)]
         [else e]))

 (define (app-application e)   
   `(make-app ,@(map app-transform (tag->expr e))))

 )

 