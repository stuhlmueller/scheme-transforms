Scheme Transforms
=================

Available transforms:

* Letrec to set!
* Assignment conversion
* Continuation-passing style (cps)
* Closure-conversion (cc)


### Sequence of transforms

The following sequence shows the interface for each transform:

    self-eval | primitive | lambda | if | (A B) | begin | set! | letrec
    
    -- letrec-to-set -->
    
    self-eval | primitive | lambda | if | (A B) | begin | set!
    
    -- assignment -->
    
    self-eval | primitive | lambda | if | (A B) | begin
    
    -- cps -->
    
    self-eval | primitive | lambda | if | (A B)
    
    -- cc -->
    
    self-eval | primitive | lambda | if | (A B)
