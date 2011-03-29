#!r6rs

(library

 (transforms)

 (export cps-transform
         cc-transform
         assignment-transform
         letrec-to-set
         redex-transform
         app-transform
         untag-transform
         transform)

 (import (rnrs)
         (transforms cps)
         (transforms cc)
         (transforms assignment)
         (transforms letrec-to-set)
         (transforms redex)
         (transforms app)
         (transforms untag))

 (define (transform expr reserved-words return-calls)
   (let* ([a (letrec-to-set expr)]
          [b (assignment-transform a)]
          [c (cps-transform b reserved-words)]
          [d (redex-transform c)]
          [e (if return-calls
                 (app-transform d)
                 (untag-transform d))]
          [f (cc-transform e reserved-words)])
     f))

 )