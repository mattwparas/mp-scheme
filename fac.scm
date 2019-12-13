(define (factorial n)
  (cond (= n 0)
      1
      (* n (factorial (- n 1)))))


(factorial 5)
(factorial 100)


(list "Hello" "my" "name" "is" "matt")