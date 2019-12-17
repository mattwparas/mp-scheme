import           Control.Monad     (unless)
import           System.IO
import Interpreter
import Control.Exception

main :: IO ()
main = do
    input <- read'
    stdlib <- (readFile "../stdlib.scm")
    unless (input == ":quit")
        $ (wrapper' input stdlib)
        -- $ print' (eval' input)
        >> main

read' :: IO String
read' = putStr "Kaye-λ> "
     >> hFlush stdout
     >> getLine

eval' :: String -> IO [String]
eval' input = eval input

print' :: IO [String] -> IO ()
print' lst = do
    res <- lst
    mapM_ putStrLn res

wrapper' :: String -> String -> IO ()
wrapper' s path =
    catch (print' (evalWithStdLib s path)) handler
    where
        handler :: SomeException -> IO ()
        handler ex = putStrLn $ "Caught Exception: " ++ show ex