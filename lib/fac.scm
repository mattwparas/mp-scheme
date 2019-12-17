(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))


(factorial 5)
(factorial 100)



(list "Hello" "my" "name" "is" "matt")

(define (fib n) 
    (if (= n 1) 
    0 
        (if (= n 2) 
            1 
            (+ (fib (- n 1)) (fib (- n 2))))))

(fib 6)
(fib 5)
(fib 15)

(factorial 5)


(define (sum2 n acc)
  (if (= n 0)
      acc
      (sum2 (- n 1) (+ n acc))))

(sum2 1000000 0)