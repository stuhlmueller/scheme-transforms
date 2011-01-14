#!r6rs

;; Return Conversion

;; Takes a program in continuation passing style and converts all
;; function calls into returns to the top-level that pass function,
;; continuation, and args back.

;; input language:
;; tag | top-level-begin-define | self-eval | primitive | lambda | if | (A B) | apply | let

;; output language:
;; tag | top-level-begin-define | self-eval | primitive | lambda | if | (A B) | apply | let

(library

 (transforms return)

 (export return-transform)

 (import (rnrs)
         (scheme-tools srfi-compat :1)
         (transforms common)
         (transforms syntax)
         (transforms utils))

 (define (return-transform e)
   (cond [(tagged? e 'call) (return-application e)]
         [(list? e) (map return-transform e)]
         [else e]))

 (define (return-application e)   
   `(return 'call ,@(map return-transform (tag->expr e))))

 )

 