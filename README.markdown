Scheme Transforms
=================

Available transforms:

* Letrec to set!
* Assignment conversion
* Continuation-passing style (cps)
* Closure-conversion (cc)


### Sequence of transforms

The following sequence shows the interface for each transform:

    tag | top-level-begin-define | self-eval | primitive | lambda | if | (A B) | begin | set! | letrec

    -- letrec-to-set -->

    tag | top-level-begin-define | self-eval | primitive | lambda | if | (A B) | begin | set!
    
    -- assignment -->

    tag | top-level-begin-define | self-eval | primitive | lambda | begin | if | (A B)
    
    -- cps -->
    
    tag | top-level-begin-define | self-eval | primitive | lambda | if | (A B) | apply | let
    
    -- redex -->

    tag | top-level-begin-define | self-eval | primitive | lambda | if | (A B) | apply | let

    -- return -->

    tag | top-level-begin-define | self-eval | primitive | lambda | if | (A B) | apply | let

    -- cc -->

    tag | top-level-begin-define | self-eval | primitive | lambda | if | (A B) | apply | let

