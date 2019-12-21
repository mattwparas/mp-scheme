



(let [(resp (get! "http://daniel-diaz.github.io/misc/pizza.json"))]
    (string->jsexpr resp))



