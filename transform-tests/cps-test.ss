#!r6rs

(import (rnrs)
        (_srfi :1)
        (transforms cps)
        (transform-tests utils)
        (transform-tests tests))

(run-tests cps-transform
           cps-eval
           (append common-test-exprs
                   cps-test-exprs))