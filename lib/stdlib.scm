{- 
Provide bindings for primitives for higher order functions
-}
(define (append x l) (append x l))
(define (cons l r) (cons l r))
(define (car pair) (car pair))
(define (cdr pair) (cdr pair))
(define (first pair) (first pair))
(define (rest pair) (rest pair))
(define (+ l r) (+ l r))
(define (- l r) (- l r))
(define (* l r) (* l r))
(define (/ l r) (/ l r))
(define (< l r) (< l r))
(define (> l r) (> l r))
(define (<= l r) (<= l r))
(define (>= l r) (>= l r))
(define (= l r) (= l r))



(define (caar pair) (car (car pair)))
(define (cadr pair) (car (cdr pair)))
(define (cdar pair) (cdr (car pair)))
(define (cddr pair) (cdr (cdr pair)))
(define (caar pair) (car (car (car pair))))
(define (caadr pair) (car (car (cdr pair))))
(define (cadar pair) (car (cdr (car pair))))
(define (caddr pair) (car (cdr (cdr pair))))
(define (cdaar pair) (cdr (car (car pair))))
(define (cdadr pair) (cdr (car (cdr pair))))
(define (cddar pair) (cdr (cdr (car pair))))
(define (cdddr pair) (cdr (cdr (cdr pair))))
(define (caaaar pair) (car (car (car (car pair)))))
(define (caaadr pair) (car (car (car (cdr pair)))))
(define (caadar pair) (car (car (cdr (car pair)))))
(define (caaddr pair) (car (car (cdr (cdr pair)))))
(define (cadaar pair) (car (cdr (car (car pair)))))
(define (cadadr pair) (car (cdr (car (cdr pair)))))
(define (caddar pair) (car (cdr (cdr (car pair)))))
(define (cadddr pair) (car (cdr (cdr (cdr pair)))))
(define (cdaaar pair) (cdr (car (car (car pair)))))
(define (cdaadr pair) (cdr (car (car (cdr pair)))))
(define (cdadar pair) (cdr (car (cdr (car pair)))))
(define (cdaddr pair) (cdr (car (cdr (cdr pair)))))
(define (cddaar pair) (cdr (cdr (car (car pair)))))
(define (cddadr pair) (cdr (cdr (car (cdr pair)))))
(define (cdddar pair) (cdr (cdr (cdr (car pair)))))
(define (cddddr pair) (cdr (cdr (cdr (cdr pair)))))
(define (id obj) obj)
(define (flip func) (lambda (arg1 arg2) (func arg2 arg1)))
(define (curry func arg1) (lambda (arg) (func arg1 arg)))
(define (compose f g) (lambda (arg) f (g arg)))

(define (foldl func accum lst)
  (if (empty? lst)
      accum
      (foldl func (func accum (car lst)) (cdr lst))))

(define (foldr func accum lst)
  (if (empty? lst)
      accum
      (func (car lst) (foldr func accum (cdr lst)))))

(define (unfold func init pred)
  (if (pred init)
      (cons init '())
      (cons init (unfold func (func init) pred))))

(define empty '())


(define (range first last)
  (if (>= first last)
      '()
      (cons first (range (+ first 1) last))))

(define (fold f a l) (foldl f a l))
(define (reduce f a l) (fold f a l))
(define (max x num-list) (fold (lambda (y z) (if (> y z) y z)) x (cons 0 num-list)))
(define (min x num-list) (fold (lambda (y z) (if (< y z) y z)) x (cons 536870911 num-list)))
(define (length lst) (fold (lambda (x y) (+ x 1)) 0 lst))


(define (reverse l) (if (empty? l) '() (append (reverse (cdr l)) (list (car l)))))


(define (mem-helper pred op) (lambda (acc next) (if (and (not acc) (pred (op next))) next acc)))
(define (memq obj lst) (fold (mem-helper (curry eq? obj) id) #f lst))
(define (memv obj lst) (fold (mem-helper (curry eqv? obj) id) #f lst))
(define (member obj lst) (fold (mem-helper (curry equal? obj) id) #f lst))
(define (assq obj alist) (fold (mem-helper (curry eq? obj) car) #f alist))
(define (assv obj alist) (fold (mem-helper (curry eqv? obj) car) #f alist))
(define (assoc obj alist) (fold (mem-helper (curry equal? obj) car) #f alist))
(define (map func lst) (foldr (lambda (x y) (cons (func x) y)) '() lst))
(define (filter pred lst) (foldr (lambda (x y) (if (pred x) (cons x y) y)) '() lst))
(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
(define (even? x) (if (= x 0) #t (odd? (- x 1))))
(define (odd? x) (if (= x 0) #f (even? (- x 1))))

(define (print! x) (println x))

(define (sum lst) (reduce + 0 lst))
(define (multiply lst) (reduce * 1 lst))


