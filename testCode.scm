(define (fib n) 
    (cond (= n 1) 
    0 
        (cond (= n 2) 
            1 
            (+ (fib (- n 1)) (fib (- n 2))))))

(fib 6)
(fib 5)
(fib 15)
; (fib 100)


(define (factorial n)
  (cond (= n 0)
      1
      (* n (factorial (- n 1)))))

(factorial 5)
