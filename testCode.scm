

(define (check-first lst) 
    (case-split 
        (first lst) 
            [7 #t] 
            [(+ 1 2 3 4) (check-first (rest lst))] 
            [else (list 1 2 3 4)]))

(check-first [7])
(check-first [10 7])
(check-first [10 2 3])
(check-first [5 5])


