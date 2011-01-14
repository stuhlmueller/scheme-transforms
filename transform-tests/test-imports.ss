#!r6rs

(library

 (transform-tests test-imports)

 (export tag)

 (import (rnrs))

 (define (tag val name)
   val)

 )