#!r6rs

(library

 (transforms)

 (export cps-transform
         cc-transform
         assignment-transform
         letrec-to-set
         redex-transform
         transform)

 (import (rnrs)
         (transforms cps)
         (transforms cc)
         (transforms assignment)
         (transforms letrec-to-set)
         (transforms redex))

 (define (transform expr reserved-words)
  (let* ([a (letrec-to-set expr)]
         [b (assignment-transform a)]
         [c (cps-transform b reserved-words)]
         [d (redex-transform c)]
         [e (cc-transform d reserved-words)])
    e))

 )