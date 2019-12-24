



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

 
