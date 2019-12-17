(define (foo x) (bar x))
(define (bar x) (foo x))
(foo 1)