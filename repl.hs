import           Control.Monad     (unless)
import           System.IO
import Scheme
import Control.Exception

main :: IO ()
main = do
    input <- read'
    unless (input == ":quit")
        $ wrapper' input
        -- $ print' (eval' input)
        >> main

read' :: IO String
read' = putStr "REPL λ> "
     >> hFlush stdout
     >> getLine

eval' :: String -> [String]
eval' input = eval input

print' :: [String] -> IO ()
print' = mapM_ putStrLn

wrapper' :: String -> IO ()
wrapper' s = catch (print' (eval s)) handler
    where
        handler :: SomeException -> IO()
        handler ex = putStrLn $ "Caught Exception: " ++ show ex