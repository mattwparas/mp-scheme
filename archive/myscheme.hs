import System.Environment   
import Data.List

import Scheme
import Control.Exception

import Data.Time




-- main = do
--     s <- readFile "somefile.txt"
--     doSomethingWith s
  
--   doSomethingWith :: String -> IO ()
--   doSomethingWith str = putStrLn str

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
    start <- getCurrentTime
    wrapper' code
    end <- getCurrentTime
    print (diffUTCTime end start)

    -- IO [String]
    -- progName <- getProgName          -- IO String
    -- putStrLn "The arguments are:"  
    -- mapM putStrLn args
    -- interpretFile (args !! 0)

    -- putStrLn "The program name is:"  
    -- putStrLn progName