import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Scheme

main :: IO ()
main = hspec $ do
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
        













