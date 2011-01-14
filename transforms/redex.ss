#!r6rs

;; eliminate unnecessary redexes introduced by cps transform

;; input language:
;; tag | top-level-begin-define | self-eval | primitive | lambda | if | (A B) | apply | let

;; output language:
;; tag | top-level-begin-define | self-eval | primitive | lambda | if | (A B) | apply | let

(library

 (transforms redex)

 (export redex-transform)

 (import (rnrs)
         (scheme-tools srfi-compat :1)
         (only (scheme-tools) all)
         (transforms common)
         (transforms syntax)
         (transforms utils))

 (define n 0)

 (define (redex-transform e)
   (set! n 0)
   (let ([e* (redex-convert e)])
     (if (= n 0)
         e*
         (redex-transform e*))))

 (define (redex-convert e)
   (cond [(lambda? e) (redex-lambda e)]
         [(list? e) (map redex-transform e)]
         [else e]))
 
 (define (variable-symbol? e)
   (and (symbol? e)
        (not (contains '(if let let* lambda define begin set! letrec) e))))

 ;; (lambda (a1 a2) (foo a1 a2))
 ;; =>
 ;; foo
 (define (redex-lambda e)
   (let ([args (lambda->args e)]
         [body (lambda->body e)])
     (if (and (list? body)
              (all variable-symbol? body)
              (equal? (rest body) args))
         (first body)
         `(lambda ,args ,(redex-convert body)))))

 )