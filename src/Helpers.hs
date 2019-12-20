module Helpers where

getAllButLast :: [a] -> [a]
getAllButLast lst = reverse (tail (reverse lst))


isInteger s = case reads s :: [(Integer, String)] of
    [(_, "")] -> True
    _         -> False
   
isDouble s = case reads s :: [(Double, String)] of
    [(_, "")] -> True
    _         -> False