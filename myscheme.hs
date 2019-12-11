import System.Environment   
import Data.List

import Scheme
import Control.Exception



-- main = do
--     s <- readFile "somefile.txt"
--     doSomethingWith s
  
--   doSomethingWith :: String -> IO ()
--   doSomethingWith str = putStrLn str

eval' :: String -> [String]
eval' input = eval input

print' :: [String] -> IO ()
print' = mapM_ putStrLn

wrapper' :: String -> IO ()
wrapper' s = catch (print' (eval s)) handler
    where
        handler :: SomeException -> IO()
        handler ex = putStrLn $ "Caught Exception: " ++ show ex



main = do  
    args <- getArgs
    code <- readFile (args !! 0)
    wrapper' code
    -- IO [String]
    -- progName <- getProgName          -- IO String
    -- putStrLn "The arguments are:"  
    -- mapM putStrLn args
    -- interpretFile (args !! 0)

    -- putStrLn "The program name is:"  
    -- putStrLn progName