module Helpers where

getAllButLast :: [a] -> [a]
getAllButLast lst = reverse (tail (reverse lst))