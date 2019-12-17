import System.Environment   
import Data.List

import Interpreter
import Control.Exception


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

main = do  
    args <- getArgs
    code <- readFile (args !! 0)
    wrapper' code
