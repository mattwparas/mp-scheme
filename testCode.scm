(define (fib n) 
    (if (= n 1) 
    0 
        (if (= n 2) 
            1 
            (+ (fib (- n 1)) (fib (- n 2))))))

(fib 6)
(fib 5)
(fib 15)

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(factorial 5)