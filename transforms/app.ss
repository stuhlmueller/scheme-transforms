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

 (define blacklist
   '(make-cell
     cell-ref
     set-cell!))

 (define (app-transform e)
   (cond [(blacklisted? e) (map app-transform (tag->expr e))]
         [(tagged? e 'app) (app-application e)]
         [(tagged? e 'abort) (app-abort e)]
         [(list? e) (map app-transform e)]
         [else e]))

 (define (blacklisted? e)
   (and (tagged? e 'app)
        (> (length (tag->expr e)) 0)
        (let ([proc (first (tag->expr e))])
          (any (lambda (name) (eq? proc name)) blacklist))))

 (define (app-application e)
   `(make-application ,@(map app-transform (tag->expr e))))

 (define (app-abort e)
   `(make-abort))

 )

 