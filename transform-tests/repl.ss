#!r6rs

(import (transforms letrec-to-set)
        (transforms assignment)
        (transforms cps)
        (transforms cc )
        (only (scheme-tools external) pretty-print console-input-port)
        (rnrs))

(define (loop)
  (display "e: ")
  (let ([e (read (console-input-port))])
    (if (equal? e 'exit)
        'done
        (begin
          (display "letrec-to-set:\n")
          (pretty-print (letrec-to-set e))
          (display "assignment:\n")
          (pretty-print (assignment-transform (letrec-to-set e)))
          (display "cps:\n")
          (pretty-print (cps-transform (assignment-transform (letrec-to-set e))))
          (display "cc:\n")
          (pretty-print (cc-transform (cps-transform (assignment-transform (letrec-to-set e)))))
          (loop)))))

(loop)