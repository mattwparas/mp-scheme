import System.Environment   
import Data.List

import Interpreter
import Control.Exception


import           Control.Monad     (unless)
import           System.IO

eval' :: String -> IO [String]
eval' input = eval input

print' :: IO [String] -> IO ()
print' lst = do
    res <- lst
    mapM_ putStrLn res


wrapper' :: String -> IO ()
wrapper' s = catch (print' (eval s)) handler
    where
        handler :: SomeException -> IO()
        handler ex = putStrLn $ "Caught Exception: " ++ show ex


main :: IO ()
main = do
    args <- getArgs
    if (length args) == 0
        then do
            putStrLn "Welcome to Kaye v1.0."
            execRepl
        else do
            code <- readFile (args !! 0)
            wrapper' code


execRepl :: IO ()
execRepl = do
    input <- read'
    stdlib <- (readFile "../lib/stdlib.scm")
    unless (input == ":quit")
        $ (wrapperRepl' input stdlib)
        -- $ print' (eval' input)
        >> execRepl

read' :: IO String
read' = putStr "Kaye-λ> "
        >> hFlush stdout
        >> getLine


wrapperRepl' :: String -> String -> IO ()
wrapperRepl' s path =
    catch (print' (evalWithStdLib s path)) handler
    where
        handler :: SomeException -> IO ()
        handler ex = putStrLn $ "Caught Exception: " ++ show ex