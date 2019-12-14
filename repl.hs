import           Control.Monad     (unless)
import           System.IO
import Scheme
import Control.Exception

main :: IO ()
main = do
    input <- read'
    stdlib <- (readFile "stdlib.scm")
    unless (input == ":quit")
        $ (wrapper' input stdlib)
        -- $ print' (eval' input)
        >> main

read' :: IO String
read' = putStr "MP λ> "
     >> hFlush stdout
     >> getLine

eval' :: String -> [String]
eval' input = eval input

print' :: [String] -> IO ()
print' = mapM_ putStrLn

wrapper' :: String -> String -> IO ()
wrapper' s path = catch (print' (evalWithStdLib s path)) handler
    where
        handler :: SomeException -> IO()
        handler ex = putStrLn $ "Caught Exception: " ++ show ex