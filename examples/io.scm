



(let [(resp (get! "http://daniel-diaz.github.io/misc/pizza.json"))
      (output (string->jsexpr resp))]
      (map (lambda (a) (println a)) output))



(let [(resp (get! "https://www.quackit.com/json/tutorial/artists.txt"))
      (output (string->jsexpr resp))]
    output)


{-
(define (pretty-print json-body)
    (case-split
        ()
    )
)
-}

(define (pretty-print json-body)
    (map (lambda (a) (println a)) json-body))


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


