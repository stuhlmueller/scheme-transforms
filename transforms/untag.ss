#!r6rs

;; Untag Transform

;; Remove all tags

;; input language:
;; tag | *

;; output language:
;; *

(library

 (transforms untag)

 (export untag-transform)

 (import (rnrs)
         (scheme-tools srfi-compat :1)
         (transforms common)
         (transforms syntax)
         (transforms utils))

 (define (untag-transform e)
   (cond [(tag? e) (untag-transform (tag->expr e))]
         [(list? e) (map untag-transform e)]
         [else e]))

 )

 