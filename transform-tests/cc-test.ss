#!r6rs

(import (rnrs)
        (transforms cc)
        (transform-tests utils)
        (transform-tests tests))

(run-tests cc-transform
           cc-eval
           (append common-test-exprs
                   cc-test-exprs))