


{-
(let [(resp (get! "http://daniel-diaz.github.io/misc/pizza.json"))]
    (string->jsexpr resp))
-}


(let [(resp (get! "https://www.quackit.com/json/tutorial/artists.txt"))
      (output (string->jsexpr resp))]
    output)






