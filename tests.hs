import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Scheme

main :: IO ()
main = hspec $ do

    -- stdLib <- (readFile "stdlib.scm")

    let stdlib = "\
            \(define (caar pair) (car (car pair)))\
            \(define (cadr pair) (car (cdr pair)))\
            \(define (cdar pair) (cdr (car pair)))\
            \(define (cddr pair) (cdr (cdr pair)))\
            \(define (caar pair) (car (car (car pair))))\
            \(define (caadr pair) (car (car (cdr pair))))\
            \(define (cadar pair) (car (cdr (car pair))))\
            \(define (caddr pair) (car (cdr (cdr pair))))\
            \(define (cdaar pair) (cdr (car (car pair))))\
            \(define (cdadr pair) (cdr (car (cdr pair))))\
            \(define (cddar pair) (cdr (cdr (car pair))))\
            \(define (cdddr pair) (cdr (cdr (cdr pair))))\
            \(define (caaaar pair) (car (car (car (car pair)))))\
            \(define (caaadr pair) (car (car (car (cdr pair)))))\
            \(define (caadar pair) (car (car (cdr (car pair)))))\
            \(define (caaddr pair) (car (car (cdr (cdr pair)))))\
            \(define (cadaar pair) (car (cdr (car (car pair)))))\
            \(define (cadadr pair) (car (cdr (car (cdr pair)))))\
            \(define (caddar pair) (car (cdr (cdr (car pair)))))\
            \(define (cadddr pair) (car (cdr (cdr (cdr pair)))))\
            \(define (cdaaar pair) (cdr (car (car (car pair)))))\
            \(define (cdaadr pair) (cdr (car (car (cdr pair)))))\
            \(define (cdadar pair) (cdr (car (cdr (car pair)))))\
            \(define (cdaddr pair) (cdr (car (cdr (cdr pair)))))\
            \(define (cddaar pair) (cdr (cdr (car (car pair)))))\
            \(define (cddadr pair) (cdr (cdr (car (cdr pair)))))\
            \(define (cdddar pair) (cdr (cdr (cdr (car pair)))))\
            \(define (cddddr pair) (cdr (cdr (cdr (cdr pair)))))\
            \(define (id obj) obj)\
            \(define (flip func) (lambda (arg1 arg2) (func arg2 arg1)))\
            \(define (curry func arg1) (lambda (arg) (func arg1 arg)))\
            \(define (compose f g) (lambda (arg) f (g arg)))\
            \(define (foldl func accum lst)\
                \(if (empty? lst)\
                \    accum\
                \    (foldl func (func accum (car lst)) (cdr lst))))\
            \(define (foldr func accum lst)\
                \(if (empty? lst)\
                \    accum\
                \    (func (car lst) (foldr func accum (cdr lst)))))\
            \(define (unfold func init pred)\
                \(if (pred init)\
                \    (cons init '())\
                \    (cons init (unfold func (func init) pred))))\
            \(define (empty) '())\
            \(define (fold f a l) (foldl f a l))\
            \(define (reduce f a l) (fold f a l))\
            \(define (max x num-list) (fold (lambda (y z) (if (> y z) y z)) x (cons 0 num-list)))\
            \(define (min x num-list) (fold (lambda (y z) (if (< y z) y z)) x (cons 536870911 num-list)))\
            \(define (length lst) (fold (lambda (x y) (+ x 1)) 0 lst))\
            \(define (reverse lst) (cdr (foldl (flip cons) '() lst)))\
            \(define (mem-helper pred op) (lambda (acc next) (if (and (not acc) (pred (op next))) next acc)))\
            \(define (memq obj lst) (fold (mem-helper (curry eq? obj) id) #f lst))\
            \(define (memv obj lst) (fold (mem-helper (curry eqv? obj) id) #f lst))\
            \(define (member obj lst) (fold (mem-helper (curry equal? obj) id) #f lst))\
            \(define (assq obj alist) (fold (mem-helper (curry eq? obj) car) #f alist))\
            \(define (assv obj alist) (fold (mem-helper (curry eqv? obj) car) #f alist))\
            \(define (assoc obj alist) (fold (mem-helper (curry equal? obj) car) #f alist))\
            \(define (map func lst) (foldr (lambda (x y) (cons (func x) y)) '() lst))\
            \(define (filter pred lst) (foldr (lambda (x y) (if (pred x) (cons x y) y)) '() lst))\
            \(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))\
            \(define (even x) (if (= x 0) #t (odd (- x 1))))\
            \(define (odd x) (if (= x 0) #f (even (- x 1))))"

    -- describe "Prelude.head" $ do
    --     it "returns the first element of a list" $ do
    --         head [23 ..] `shouldBe` (23 :: Int)

    --     it "returns the first element of an *arbitrary* list" $
    --         property $ \x xs -> head (x:xs) == (x :: Int)

    --     it "throws an exception if used with an empty list" $ do
    --         evaluate (head []) `shouldThrow` anyException

    describe "Scheme" $ do
        it "evaluates an addition statement" $ do
            eval "(+ 5 5)" `shouldBe` ["10"]

        it "evaluates an subtraction statement" $ do
            eval "(- 5 5)" `shouldBe` ["0"]

        it "evaluates a multiplication statement" $ do
            eval "(* 5 5)" `shouldBe` ["25"]

        it "evaluates a division statement" $ do
            eval "(/ 10 2)" `shouldBe` ["5"]

        it "evaluates an if statement - true" $ do
            eval "(if #t #t #f)" `shouldBe` ["#t"]
        
        it "evaluates an if statement - false" $ do
            eval "(if #f #f #t)" `shouldBe` ["#t"]

        it "case-split statement" $ do
            eval "(define (check-first lst) \
            \ (case-split \
              \  (first lst) \
                 \   [7 #t] \
                    \[(+ 1 2 3 4) (check-first (rest lst))] \
                    \[else (list 1 2 3 4)]))\
        \ (check-first [7]) \
        \ (check-first [10 7]) \
        \ (check-first [10 2 3]) \
        \ (check-first [5 5])" `shouldBe` ["#t", "#t", "'(1 2 3 4)", "'(1 2 3 4)"]

        it "factorial" $ do
            eval "(define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1))))) \
                    \ (factorial 5) (factorial 6)" `shouldBe` ["120", "720"]

        it "map" $ do
            eval (stdlib ++ "(map (lambda (a) (+ a 5)) (list 1 2 3 4 5))") `shouldBe` ["'(6 7 8 9 10)"]
        
        it "filter" $ do
            eval (stdlib ++ "(filter (lambda (a) (= a 5)) (list 5 2 4 3 5))") `shouldBe` ["'(5 5)"]

        it "Functions without arguments" $ do
            eval (stdlib ++ "(empty)") `shouldBe` ["'()"]

        it "Functions without arguments lambda" $ do
            eval ("(lambda () 5)") `shouldBe` ["internal function"]

        it "Function application without arguments" $ do
            eval ("((lambda () 5))") `shouldBe` ["5"]

        it "cond with multiple cases - else case" $ do
            eval "(cond [(= 2 3) \"case one\"] \
                    \   [(= 3 4) \"case two\"] \
                    \   [else \"else\"])" `shouldBe` ["\"else\""]

        it "cond with multiple cases - first case" $ do
            eval "(cond [(= 2 2) \"case one\"] \
                    \   [(= 3 4) \"case two\"] \
                    \   [else \"else\"])" `shouldBe` ["\"case one\""]

        it "cond with multiple cases - first case" $ do
            eval "(cond [(= 2 3) \"case one\"] \
                    \   [(= 3 3) \"case two\"] \
                    \   [else \"else\"])" `shouldBe` ["\"case two\""]

        it "list with tick notation" $ do
            eval "'(1 2 3 4)" `shouldBe` ["'(1 2 3 4)"]

        it "list with list notation" $ do
            eval "(list 1 2 3 4)" `shouldBe` ["'(1 2 3 4)"]

        it "list with bracket notation" $ do
            eval "[1 2 3 4]" `shouldBe` ["'(1 2 3 4)"]

        it "Addition with multiple arguments" $ do
            eval "(+ 1 2 3 4 5)" `shouldBe` ["15"]

        it "Subtraction with multiple arguments" $ do
            eval "(- 10 5 3 2)" `shouldBe` ["0"]

        it "Multiplication with multiple arguments" $ do
            eval "(* 1 2 3 4 5)" `shouldBe` ["120"]

        it "Division with multiple arguments, odd" $ do
            eval "(/ 100 10 5)" `shouldBe` ["2"]

        it "Division with multiple arguments, even" $ do
            eval "(/ 1000 10 10 5)" `shouldBe` ["2"]

        it "AND with multiple arguments" $ do
            eval "(and #t #t #t #t)" `shouldBe` ["#t"]

        it "AND with muliple arguments, different notations" $ do
            eval "(and #t #t #true #t)" `shouldBe` ["#t"]

        it "AND with muliple arguments, different notations, odd numbered arguments" $ do
            eval "(and #t #t #t)" `shouldBe` ["#t"]

        it "AND with multiple arguments" $ do
            eval "(and #t #t #false #t)" `shouldBe` ["#f"]

        it "OR with multiple arguments" $ do
            eval "(or #t #f #t)" `shouldBe` ["#t"]

        it "OR with multiple arguments" $ do
            eval "(or #f #f #f #f)" `shouldBe` ["#f"]

        it "< with 2 arguments, true" $ do
            eval "(< 10 1000)" `shouldBe` ["#t"]

        it "< with 2 arguments, false" $ do
            eval "(< 1000 10)" `shouldBe` ["#f"]

        it "< with multiple arguments, 5, true" $ do
            eval "(< 0 1 2 3 4)" `shouldBe` ["#t"]

        it "< with multiple arguments, 4, true" $ do
            eval "(< 0 1 2 4)" `shouldBe` ["#t"]

        it "< with multiple arguments, 3, false" $ do
            eval "(< 1 10 2)" `shouldBe` ["#f"]

        it "< with multiple arguments, 4, false" $ do
            eval "(< 100 0 1)" `shouldBe` ["#f"]

        it "> with 2 arguments, false" $ do
            eval "(> 10 1000)" `shouldBe` ["#f"]

        it "> with 2 arguments, true" $ do
            eval "(> 1000 10)" `shouldBe` ["#t"]

        it "> with multiple arguments, 5, false" $ do
            eval "(> 0 1 2 3 4)" `shouldBe` ["#f"]

        it "> with multiple arguments, 4, false" $ do
            eval "(> 0 1 2 4)" `shouldBe` ["#f"]

        it "> with multiple arguments, 3, true" $ do
            eval "(> 100 10 2)" `shouldBe` ["#t"]

        it "> with multiple arguments, 4, true" $ do
            eval "(> 100 2 1)" `shouldBe` ["#t"]

        it "<= with 2 arguments, true" $ do
            eval "(<= 10 1000)" `shouldBe` ["#t"]

        it "<= with 2 arguments, true, equal" $ do
            eval "(<= 10 10)" `shouldBe` ["#t"]

        it "<= with 2 arguments, false" $ do
            eval "(<= 1000 10)" `shouldBe` ["#f"]

        it "<= with multiple arguments, 5, true" $ do
            eval "(<= 0 1 2 3 4)" `shouldBe` ["#t"]

        it "<= with multiple arguments, 4, true" $ do
            eval "(<= 0 1 2 4)" `shouldBe` ["#t"]

        it "<= with multiple arguments, 3, false" $ do
            eval "(<= 1 10 2)" `shouldBe` ["#f"]

        it "<= with multiple arguments, 4, false" $ do
            eval "(<= 100 0 1 3)" `shouldBe` ["#f"]

        it ">= with 2 arguments, true" $ do
            eval "(>= 1000 10)" `shouldBe` ["#t"]

        it ">= with 2 arguments, true, equal" $ do
            eval "(>= 10 10)" `shouldBe` ["#t"]

        it ">= with 2 arguments, true" $ do
            eval "(>= 1000 10)" `shouldBe` ["#t"]

        it ">= with multiple arguments, 5, false" $ do
            eval "(>= 0 1 2 3 4)" `shouldBe` ["#f"]

        it ">= with multiple arguments, 4, false" $ do
            eval "(>= 0 1 2 4)" `shouldBe` ["#f"]

        it ">= with multiple arguments, 3, true" $ do
            eval "(>= 100 10 2)" `shouldBe` ["#t"]

        it ">= with multiple arguments, 4, true" $ do
            eval "(>= 100 10 1 0)" `shouldBe` ["#t"]

        it "= with multiple arguments, 3, true" $ do
            eval "(= 3 3 3)" `shouldBe` ["#t"]
            
        it "= with multiple arguments, 4, true" $ do
            eval "(= 3 3 3 3)" `shouldBe` ["#t"]

        it "= with multiple arguments, 3, false" $ do
            eval "(= 3 3 4)" `shouldBe` ["#f"]

        it "= with multiple arguments, 4, false" $ do
            eval "(= 3 3 3 4)" `shouldBe` ["#f"]

        it "first" $ do
            eval "(first '(1 2 3 4))" `shouldBe` ["1"]
        
        it "rest" $ do
            eval "(rest '(1 2 3 4))" `shouldBe` ["'(2 3 4)"]

        it "cons" $ do
            eval "(cons 1 '(2 3 4))" `shouldBe` ["'(1 2 3 4)"]

        it "append" $ do
            eval "(append [1 2] [3 4])" `shouldBe` ["'(1 2 3 4)"]