import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Interpreter

{-


runhaskell -i/Users/mwparas/Documents/schemefun/mp-scheme/src tests.hs

-}

main :: IO ()
main = hspec $ do

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

    -- stdlib <- readFile "stdlib.scm"

    -- describe "Prelude.head" $ do
    --     it "returns the first element of a list" $ do
    --         head [23 ..] `shouldBe` (23 :: Int)

    --     it "returns the first element of an *arbitrary* list" $
    --         property $ \x xs -> head (x:xs) == (x :: Int)

    --     it "throws an exception if used with an empty list" $ do
    --         evaluate (head []) `shouldThrow` anyException

    describe "Scheme" $ do
        it "evaluates an addition statement" $ do
            res <- eval "(+ 5 5)"
            res `shouldBe` ["10"]

        it "evaluates an subtraction statement" $ do
            res <- eval "(- 5 5)" 
            res `shouldBe` ["0"]

        it "evaluates a multiplication statement" $ do
            res <- eval "(* 5 5)" 
            res `shouldBe` ["25"]

        it "evaluates a division statement" $ do
            res <- eval "(/ 10 2)" 
            res `shouldBe` ["5"]

        it "evaluates an if statement - true" $ do
            res <- eval "(if #t #t #f)" 
            res `shouldBe` ["#t"]
        
        it "evaluates an if statement - false" $ do
            res <- eval "(if #f #f #t)" 
            res `shouldBe` ["#t"]

        it "case-split statement" $ do
            res <- eval "(define (check-first lst) \
            \ (case-split \
              \  (first lst) \
                 \   [7 #t] \
                    \[(+ 1 2 3 4) (check-first (rest lst))] \
                    \[else (list 1 2 3 4)]))\
            \ (check-first [7]) \
            \ (check-first [10 7]) \
            \ (check-first [10 2 3]) \
            \ (check-first [5 5])" 
            res `shouldBe` ["#t", "#t", "'(1 2 3 4)", "'(1 2 3 4)"]

        it "factorial" $ do
            res <- eval "(define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1))))) \
                    \ (factorial 5) (factorial 6)" 
            res `shouldBe` ["120", "720"]

        it "map" $ do
            res <- eval (stdlib ++ "(map (lambda (a) (+ a 5)) (list 1 2 3 4 5))") 
            res `shouldBe` ["'(6 7 8 9 10)"]
        
        it "filter" $ do
            res <- eval (stdlib ++ "(filter (lambda (a) (= a 5)) (list 5 2 4 3 5))") 
            res `shouldBe` ["'(5 5)"]

        it "Anonymous function application" $ do
            res <- eval "((λ (a) (+ a 5)) 10)"
            res `shouldBe` ["15"]

        it "Anonymous function definition" $ do
            res <- eval "(λ (a) (+ a 5))"
            res `shouldBe` ["internal function"]

        it "Functions without arguments" $ do
            res <- eval (stdlib ++ "(empty)") 
            res `shouldBe` ["'()"]

        it "Functions without arguments lambda" $ do
            res <- eval ("(lambda () 5)") 
            res `shouldBe` ["internal function"]

        it "Function application without arguments" $ do
            res <- eval ("((lambda () 5))") 
            res `shouldBe` ["5"]

        it "cond with multiple cases - else case" $ do
            res <- eval "(cond [(= 2 3) \"case one\"] \
                    \   [(= 3 4) \"case two\"] \
                    \   [else \"else\"])" 
            res `shouldBe` ["\"else\""]

        it "cond with multiple cases - first case" $ do
            res <- eval "(cond [(= 2 2) \"case one\"] \
                    \   [(= 3 4) \"case two\"] \
                    \   [else \"else\"])" 
            res `shouldBe` ["\"case one\""]

        it "cond with multiple cases - first case" $ do
            res <- eval "(cond [(= 2 3) \"case one\"] \
                    \   [(= 3 3) \"case two\"] \
                    \   [else \"else\"])" 
            res `shouldBe` ["\"case two\""]

        it "list with tick notation" $ do
            res <- eval "'(1 2 3 4)" 
            res `shouldBe` ["'(1 2 3 4)"]

        it "list with list notation" $ do
            res <- eval "(list 1 2 3 4)" 
            res `shouldBe` ["'(1 2 3 4)"]

        it "list with bracket notation" $ do
            res <- eval "[1 2 3 4]" 
            res `shouldBe` ["'(1 2 3 4)"]

        it "Addition with multiple arguments" $ do
            res <- eval "(+ 1 2 3 4 5)" 
            res `shouldBe` ["15"]

        it "Subtraction with multiple arguments" $ do
            res <- eval "(- 10 5 3 2)" 
            res `shouldBe` ["0"]

        it "Multiplication with multiple arguments" $ do
            res <- eval "(* 1 2 3 4 5)" 
            res `shouldBe` ["120"]

        it "Division with multiple arguments, odd" $ do
            res <- eval "(/ 100 10 5)" 
            res `shouldBe` ["2"]

        it "Division with multiple arguments, even" $ do
            res <- eval "(/ 1000 10 10 5)" 
            res `shouldBe` ["2"]

        it "AND with multiple arguments" $ do
            res <- eval "(and #t #t #t #t)" 
            res `shouldBe` ["#t"]

        it "AND with muliple arguments, different notations" $ do
            res <- eval "(and #t #t #true #t)" 
            res `shouldBe` ["#t"]

        it "AND with muliple arguments, different notations, odd numbered arguments" $ do
            res <- eval "(and #t #t #t)" 
            res `shouldBe` ["#t"]

        it "AND with multiple arguments" $ do
            res <- eval "(and #t #t #false #t)" 
            res `shouldBe` ["#f"]

        it "OR with multiple arguments" $ do
            res <- eval "(or #t #f #t)" 
            res `shouldBe` ["#t"]

        it "OR with multiple arguments" $ do
            res <- eval "(or #f #f #f #f)" 
            res `shouldBe` ["#f"]

        it "< with 2 arguments, true" $ do
            res <- eval "(< 10 1000)" 
            res `shouldBe` ["#t"]

        it "< with 2 arguments, false" $ do
            res <- eval "(< 1000 10)" 
            res `shouldBe` ["#f"]

        it "< with multiple arguments, 5, true" $ do
            res <- eval "(< 0 1 2 3 4)" 
            res `shouldBe` ["#t"]

        it "< with multiple arguments, 4, true" $ do
            res <- eval "(< 0 1 2 4)" 
            res `shouldBe` ["#t"]

        it "< with multiple arguments, 3, false" $ do
            res <- eval "(< 1 10 2)" 
            res `shouldBe` ["#f"]

        it "< with multiple arguments, 4, false" $ do
            res <- eval "(< 100 0 1)" 
            res `shouldBe` ["#f"]

        it "> with 2 arguments, false" $ do
            res <- eval "(> 10 1000)" 
            res `shouldBe` ["#f"]

        it "> with 2 arguments, true" $ do
            res <- eval "(> 1000 10)" 
            res `shouldBe` ["#t"]

        it "> with multiple arguments, 5, false" $ do
            res <- eval "(> 0 1 2 3 4)" 
            res `shouldBe` ["#f"]

        it "> with multiple arguments, 4, false" $ do
            res <- eval "(> 0 1 2 4)" 
            res `shouldBe` ["#f"]

        it "> with multiple arguments, 3, true" $ do
            res <- eval "(> 100 10 2)" 
            res `shouldBe` ["#t"]

        it "> with multiple arguments, 4, true" $ do
            res <- eval "(> 100 2 1)" 
            res `shouldBe` ["#t"]

        it "<= with 2 arguments, true" $ do
            res <- eval "(<= 10 1000)" 
            res `shouldBe` ["#t"]

        it "<= with 2 arguments, true, equal" $ do
            res <- eval "(<= 10 10)" 
            res `shouldBe` ["#t"]

        it "<= with 2 arguments, false" $ do
            res <- eval "(<= 1000 10)" 
            res `shouldBe` ["#f"]

        it "<= with multiple arguments, 5, true" $ do
            res <- eval "(<= 0 1 2 3 4)" 
            res `shouldBe` ["#t"]

        it "<= with multiple arguments, 4, true" $ do
            res <- eval "(<= 0 1 2 4)" 
            res `shouldBe` ["#t"]

        it "<= with multiple arguments, 3, false" $ do
            res <- eval "(<= 1 10 2)" 
            res `shouldBe` ["#f"]

        it "<= with multiple arguments, 4, false" $ do
            res <- eval "(<= 100 0 1 3)" 
            res `shouldBe` ["#f"]

        it ">= with 2 arguments, true" $ do
            res <- eval "(>= 1000 10)" 
            res `shouldBe` ["#t"]

        it ">= with 2 arguments, true, equal" $ do
            res <- eval "(>= 10 10)" 
            res `shouldBe` ["#t"]

        it ">= with 2 arguments, true" $ do
            res <- eval "(>= 1000 10)" 
            res `shouldBe` ["#t"]

        it ">= with multiple arguments, 5, false" $ do
            res <- eval "(>= 0 1 2 3 4)" 
            res `shouldBe` ["#f"]

        it ">= with multiple arguments, 4, false" $ do
            res <- eval "(>= 0 1 2 4)" 
            res `shouldBe` ["#f"]

        it ">= with multiple arguments, 3, true" $ do
            res <- eval "(>= 100 10 2)" 
            res `shouldBe` ["#t"]

        it ">= with multiple arguments, 4, true" $ do
            res <- eval "(>= 100 10 1 0)" 
            res `shouldBe` ["#t"]

        it "= with multiple arguments, 3, true" $ do
            res <- eval "(= 3 3 3)" 
            res `shouldBe` ["#t"]
            
        it "= with multiple arguments, 4, true" $ do
            res <- eval "(= 3 3 3 3)" 
            res `shouldBe` ["#t"]

        it "= with multiple arguments, 3, false" $ do
            res <- eval "(= 3 3 4)" 
            res `shouldBe` ["#f"]

        it "= with multiple arguments, 4, false" $ do
            res <- eval "(= 3 3 3 4)" 
            res `shouldBe` ["#f"]

        it "first" $ do
            res <- eval "(first '(1 2 3 4))" 
            res `shouldBe` ["1"]
        
        it "rest" $ do
            res <- eval "(rest '(1 2 3 4))" 
            res `shouldBe` ["'(2 3 4)"]

        it "cons" $ do
            res <- eval "(cons 1 '(2 3 4))" 
            res `shouldBe` ["'(1 2 3 4)"]

        it "append" $ do
            res <- eval "(append [1 2] [3 4])" 
            res `shouldBe` ["'(1 2 3 4)"]

        it "empty stdlib" $ do
            res <- eval (stdlib ++ "(empty)")
            res `shouldBe` ["'()"]

        it "string to list" $ do
            let statement = "(string->list \"hello\")"
            res <- eval statement
            res `shouldBe` ["'(#/h #/e #/l #/l #/o)"]

        it "list to string" $ do
            let statement = "(list->string '(#/h #/e #/l #/l #/o))"
            res <- eval statement
            res `shouldBe` ["\"hello\""]

        it "string->list, list->string is deterministic" $ do
            let statement = "(list->string (string->list \"hello\"))"
            res <- eval statement
            res `shouldBe` ["\"hello\""]

        it "space character" $ do
            let statement = "#/ "
            res <- eval statement
            res `shouldBe` ["#/space"]

        it "Double" $ do
            let statement = "10.0"
            res <- eval statement
            res `shouldBe` ["10.0"]

        it "Double addition, left double" $ do
            let statement = "(+ 10.0 10)"
            res <- eval statement
            res `shouldBe` ["20.0"]

        it "Double addition, right double" $ do
            let statement = "(+ 10 10.0)"
            res <- eval statement
            res `shouldBe` ["20.0"]

        it "Double addition, both double" $ do
            let statement = "(+ 10.0 10.0)"
            res <- eval statement
            res `shouldBe` ["20.0"]

        it "Double subtraction, left double" $ do
            let statement = "(- 20.0 10)"
            res <- eval statement
            res `shouldBe` ["10.0"]

        it "Double subtraction, right double" $ do
            let statement = "(- 20 10.0)"
            res <- eval statement
            res `shouldBe` ["10.0"]
        
        it "Double subtraction, both double" $ do
            let statement = "(- 20.0 5.0)"
            res <- eval statement
            res `shouldBe` ["15.0"]

        it "Double multiplication, left double" $ do
            let statement = "(* 2.0 5)"
            res <- eval statement
            res `shouldBe` ["10.0"]

        it "Double multiplication, right double" $ do
            let statement = "(* 5 2.0)"
            res <- eval statement
            res `shouldBe` ["10.0"]
        
        it "Double multiplication, both double" $ do
            let statement = "(* 5.0 2.0)"
            res <- eval statement
            res `shouldBe` ["10.0"]

        it "Double division, left double" $ do
            let statement = "(/ 10.0 5)"
            res <- eval statement
            res `shouldBe` ["2.0"]

        it "Double division, right double" $ do
            let statement = "(/ 10 2.0)"
            res <- eval statement
            res `shouldBe` ["5.0"]
        
        it "Double division, both double" $ do
            let statement = "(/ 10.0 2.0)"
            res <- eval statement
            res `shouldBe` ["5.0"]
        
        it "first on a string" $ do
            let statement = "(first \"apple\")"
            res <- eval statement
            res `shouldBe` ["#/a"]

        it "rest on a string" $ do
            let statement = "(rest \"apple\")"
            res <- eval statement
            res `shouldBe` ["\"pple\""]

        it "empty? on a string - true" $ do
            let statement = stdlib ++ "(empty? \"\")"
            res <- eval statement
            res `shouldBe` ["#t"]
        
        it "empty? on a string - false" $ do
            let statement = stdlib ++ "(empty? \"not empty string\")"
            res <- eval statement
            res `shouldBe` ["#f"]
        
        it "length on a string" $ do
            let statement = stdlib ++ "(length \"test\")"
            res <- eval statement
            res `shouldBe` ["4"]

        it "< for doubles" $ do
            let statement = "(< 2.0 5.0)"
            res <- eval statement
            res `shouldBe` ["#t"]

        it "< for doubles and ints" $ do
            let statement = "(< 2.0 5)"
            res <- eval statement
            res `shouldBe` ["#t"]

        it "<= for doubles" $ do
            let statement = "(<= 2.0 5.0)"
            res <- eval statement
            res `shouldBe` ["#t"]

        it "<= for doubles" $ do
            let statement = "(<= 2.0 2.0)"
            res <- eval statement
            res `shouldBe` ["#t"]

        it "<= for doubles and ints" $ do
            let statement = "(<= 2.0 5)"
            res <- eval statement
            res `shouldBe` ["#t"]

        it "> for doubles" $ do
            let statement = "(> 5.0 2.0)"
            res <- eval statement
            res `shouldBe` ["#t"]

        it "> for doubles and ints" $ do
            let statement = "(> 5.0 2)"
            res <- eval statement
            res `shouldBe` ["#t"]

        it ">= for doubles" $ do
            let statement = "(>= 5.0 2.0)"
            res <- eval statement
            res `shouldBe` ["#t"]

        it ">= for doubles and ints" $ do
            let statement = "(>= 5.0 2)"
            res <- eval statement
            res `shouldBe` ["#t"]

        it ">= for doubles and ints" $ do
            let statement = "(>= 5.0 5.0)"
            res <- eval statement
            res `shouldBe` ["#t"]

        it "string? - true" $ do
            let statement = "(string? \"hello\")"
            res <- eval statement
            res `shouldBe` ["#t"]

        it "string? - false" $ do
            let statement = "(string? '(1 2 3 4))"
            res <- eval statement
            res `shouldBe` ["#f"]

        it "list? - true" $ do
            let statement = "(list? '(1 2 3 4))"
            res <- eval statement
            res `shouldBe` ["#t"]
        
        it "list? - false" $ do
            let statement = "(list? 4)"
            res <- eval statement
            res `shouldBe` ["#f"]

        it "list? - false" $ do
            let statement = "(list? \"test\")"
            res <- eval statement
            res `shouldBe` ["#f"]

        it "number? - true" $ do
            let statement = "(number? 1)"
            res <- eval statement
            res `shouldBe` ["#t"]

        it "number? - true" $ do
            let statement = "(number? 1.0)"
            res <- eval statement
            res `shouldBe` ["#t"]

        it "double? - false" $ do
            let statement = "(double? 1)"
            res <- eval statement
            res `shouldBe` ["#f"]

        it "double? - true" $ do
            let statement = "(double? 1.0)"
            res <- eval statement
            res `shouldBe` ["#t"]

        it "integer? - true" $ do
            let statement = "(integer? 1)"
            res <- eval statement
            res `shouldBe` ["#t"]

        it "integer? - false" $ do
            let statement = "(integer? 1.0)"
            res <- eval statement
            res `shouldBe` ["#f"]

        it "bool? - true" $ do
            let statement = "(bool? #t) (bool? #f)"
            res <- eval statement
            res `shouldBe` ["#t", "#t"]

        it "bool? - false" $ do
            let statement = "(bool? 2) (bool? #/a) (bool? \"f\") (bool? \"#f\")"
            res <- eval statement
            res `shouldBe` ["#f", "#f", "#f", "#f"]

        it "closure? - true" $ do
            let statement = "(closure? (lambda (a) 5))"
            res <- eval statement
            res `shouldBe` ["#t"]

        it "closure? - false" $ do
            let statement = "(closure? 5) \
            \ (closure? #/a) (closure? 5.0) (closure? \"string\") (closure? '(1 2 3 4))"
            res <- eval statement
            res `shouldBe` ["#f", "#f", "#f", "#f", "#f"]

        it "char? - true" $ do
            let statement = "(char? #/a)"
            res <- eval statement
            res `shouldBe` ["#t"]

        it "char? - false" $ do
            let statement = "(char? 5) (char? 5.0) \
            \ (char? \"string\") (char? '(1 2 3 4)) (char? (lambda () 5))"
            res <- eval statement
            res `shouldBe` ["#f", "#f", "#f", "#f", "#f"]

        it "println" $ do
            let statement = "(println 5)"
            res <- eval statement
            res `shouldBe` ["null"]
