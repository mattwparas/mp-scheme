module Primitives where

import DataDefinitions
import Helpers

import Control.Exception hiding (handle, try)
import System.IO as SIO

import Control.Monad.IO.Class
import Control.Monad.Reader

import Data.Text.IO as TIO
import Data.Text as T hiding (last, unwords, map, tail, head, length, reverse, filter, try, take, zip)

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Exception hiding (handle, try)
import System.Directory
import System.IO as SIO
import Control.Monad.Reader
import Network.HTTP
import Network.HTTP.Conduit
import Control.Concurrent

import Data.ByteString.Lazy.Char8 as Char8 (unpack)


-- import Data.ByteString.Char8 as C8 (unpack)
-- import Data.ByteString.Lazy.Internal.ByteString as C8 (unpack)

-- import Data.ByteString.Char8 as C8 (unpack)
-- import qualified Data.ByteString.Lazy as LBS
-- import qualified Data.ByteString.Char8 as BS

charOp :: ExprValue -> Char
charOp (CharV c) = c
charOp e = error ("Char operation applied to non char: " ++ (show e))

getStructHelper :: String -> ExprValue -> ExprValue
getStructHelper name (StructV []) = error ("Key not found in struct: " ++ name)
getStructHelper name (StructV (x:xs)) = 
    if name == (fst x)
        then (snd x)
        else getStructHelper name (StructV xs)
getStructHelper _ _ = error "malformed getStructHelper"

checkType :: ExprValue -> ExprValueT -> ExprValue
checkType (NumV _) (IntT) = BoolV True
checkType (DoubV _) (DoubT) = BoolV True
checkType (NumV _) (NumberT) = BoolV True
checkType (DoubV _) (NumberT) = BoolV True
checkType (ClosureV _ _ _) (ClosureT) = BoolV True
checkType (ListV _) (ListT) = BoolV True
checkType (CharV _) (CharT) = BoolV True
checkType (BoolV _) (BoolT) = BoolV True
checkType (StructV _) (StructT) = BoolV True
checkType (StringV _) (StringT) = BoolV True
checkType _ _ = BoolV False

-- TODO finish this up for all cases
castType :: ExprValue -> ExprValueT -> ExprValue
castType (StringV s) (IntT) = 
    if isInteger s
        then (NumV (read s::Integer))
        else error ("Cannot cast value from string to integer: " ++ s)
castType (StringV s) (DoubT) = 
    if isDouble s
        then (DoubV (read s::Double))
        else error ("Cannot cast value from string to double: " ++ s)
castType (StringV s) (NumberT) =
    if isInteger s
        then (NumV (read s::Integer))
        else if isDouble s
            then (DoubV (read s::Double))
            else error ("Cannot cast value from string to number: " ++ s)
castType (StringV s) (ListT) = (ListV (map (\x -> (CharV x)) s))
castType (ListV l) (StringT) = (StringV (map charOp l))
castType (NumV n) (DoubT) = (DoubV (fromIntegral n))
castType (DoubV n) (IntT) = (NumV (round n))
castType l r = error ("not implemented for: " ++ (show r))


numOpAdd :: ExprValue -> ExprValue -> ExprValue
numOpAdd (NumV l) (NumV r) = (NumV (l + r))
numOpAdd (DoubV l) (DoubV r) = (DoubV (l + r))
numOpAdd (NumV l) (DoubV r) = (DoubV ((fromIntegral l) + r))
numOpAdd (DoubV l) (NumV r) = (DoubV (l + (fromIntegral r)))
numOpAdd _ _ = (error "Wrong value given to addition")

numOpSub :: ExprValue -> ExprValue -> ExprValue
numOpSub (NumV l) (NumV r) = (NumV (l - r))
numOpSub (DoubV l) (DoubV r) = (DoubV (l - r))
numOpSub (NumV l) (DoubV r) = (DoubV ((fromIntegral l) - r))
numOpSub (DoubV l) (NumV r) = (DoubV (l - (fromIntegral r)))
numOpSub _ _ = (error "Wrong value given to subtraction")

numOpMult :: ExprValue -> ExprValue -> ExprValue
numOpMult (NumV l) (NumV r) = (NumV (l * r))
numOpMult (DoubV l) (DoubV r) = (DoubV (l * r))
numOpMult (NumV l) (DoubV r) = (DoubV ((fromIntegral l) * r))
numOpMult (DoubV l) (NumV r) = (DoubV (l * (fromIntegral r)))
numOpMult _ _ = (error "Wrong value given to multiplication")

numOpDiv :: ExprValue -> ExprValue -> ExprValue
numOpDiv (NumV l) (NumV r) = (NumV (l `div` r))
numOpDiv (DoubV l) (DoubV r) = (DoubV (l / r))
numOpDiv (NumV l) (DoubV r) = (DoubV ((fromIntegral l) / r))
numOpDiv (DoubV l) (NumV r) = (DoubV (l / (fromIntegral r)))
numOpDiv _ _ = (error "Wrong value given to division")

numOpCompLt :: ExprValue -> ExprValue -> ExprValue
numOpCompLt (NumV l) (NumV r) = (BoolV (l < r))
numOpCompLt (DoubV l) (DoubV r) = (BoolV (l < r))
numOpCompLt (NumV l) (DoubV r) = (BoolV ((fromIntegral l) < r))
numOpCompLt (DoubV l) (NumV r) = (BoolV (l < (fromIntegral r)))
numOpCompLt _ _ = (error "Wrong value given to less than")

numOpCompGt :: ExprValue -> ExprValue -> ExprValue
numOpCompGt (NumV l) (NumV r) = (BoolV (l > r))
numOpCompGt (DoubV l) (DoubV r) = (BoolV (l > r))
numOpCompGt (NumV l) (DoubV r) = (BoolV ((fromIntegral l) > r))
numOpCompGt (DoubV l) (NumV r) = (BoolV (l > (fromIntegral r)))
numOpCompGt _ _ = (error "Wrong value given to greater than")

numOpCompLtE :: ExprValue -> ExprValue -> ExprValue
numOpCompLtE (NumV l) (NumV r) = (BoolV (l <= r))
numOpCompLtE (DoubV l) (DoubV r) = (BoolV (l <= r))
numOpCompLtE (NumV l) (DoubV r) = (BoolV ((fromIntegral l) <= r))
numOpCompLtE (DoubV l) (NumV r) = (BoolV (l <= (fromIntegral r)))
numOpCompLtE _ _ = (error "Wrong value given to less than or equal")

numOpCompGtE :: ExprValue -> ExprValue -> ExprValue
numOpCompGtE (NumV l) (NumV r) = (BoolV (l >= r))
numOpCompGtE (DoubV l) (DoubV r) = (BoolV (l >= r))
numOpCompGtE (NumV l) (DoubV r) = (BoolV ((fromIntegral l) >= r))
numOpCompGtE (DoubV l) (NumV r) = (BoolV (l >= (fromIntegral r)))
numOpCompGtE _ _ = (error "Wrong value given to greater than or equal")


firstOp :: ExprValue -> ExprValue
firstOp (ListV lst) = (head lst)
firstOp (StringV str) = CharV (head str)
firstOp e = error ("First applied to non list or string: " ++ (show e))

restOp :: ExprValue -> ExprValue
restOp (ListV lst) = ListV (tail lst)
restOp (StringV str) = StringV (tail str)
restOp e = error ("Rest applied to non list or string: " ++ (show e))

emptyOp :: ExprValue -> ExprValue
emptyOp (ListV lst) = (BoolV ((length lst) == 0))
emptyOp (StringV str) = (BoolV ((length str) == 0))
emptyOp e = error ("Empty? applied to non list or string: " ++ (show e))

appendOp :: ExprValue -> ExprValue -> ExprValue
appendOp (ListV l) (ListV r) = (ListV (l ++ r))
appendOp (StringV l) (StringV r) = (StringV (l ++ r))
appendOPp l r = error ("Append used between non matching lists or strings: " ++ (show l) ++ " " ++ (show r))

boolOp :: ExprValue -> Bool
boolOp (BoolV b) = b
boolOp _ = error "bool operation applied to non bool"

boolToString :: Bool -> String
boolToString True = "#t"
boolToString False = "#f"

matchStrToBool :: String -> Bool
matchStrToBool "#t" = True
matchStrToBool "#f" = False
matchStrToBool "#true" = True
matchStrToBool "#false" = False
matchStrToBool "#T" = True
matchStrToBool "#F" = False
matchStrToBool "#True" = True
matchStrToBool "#False" = False
matchStrToBool "true" = True
matchStrToBool "false" = False
matchStrToBool _ = error "Boolean malformed"

-- TODO get rid of this?
evalEquality :: ExprValue -> ExprValue -> ExprValue
evalEquality (NumV l) (NumV r) = BoolV (l == r)
evalEquality (BoolV l) (BoolV r) = BoolV (l == r)
evalEquality _ _ = BoolV False

evalTestBool :: ExprValue -> Bool
evalTestBool (BoolV b) = b
evalTestBool _ = error "Invalid bool Value"

listOpV :: ExprValue -> [ExprValue]
listOpV (ListV lst) = lst
listOpV e = error ("List operation applied to non list: " ++ (show e))

charFormatting :: Char -> String
charFormatting ' ' = "space"
charFormatting c = [c]

slurp :: ExprValue -> Eval ExprValue
slurp (StringV txt) = liftIO $ readTextToStringV txt
slurp val = error ("read expects string, instead got: " ++ (show val))


readTextToStringV :: String -> IO ExprValue
readTextToStringV path = do
    exists <- doesFileExist $ path
    if exists
        then (SIO.readFile path) >>= (return . StringV)
        else error (" file does not exist: " ++ path)

userInput :: Eval ExprValue
userInput = liftIO $ userInputToStringV

userInputToStringV :: IO ExprValue
userInputToStringV = do
    SIO.getLine >>= (return . StringV)


printLn :: ExprValue -> Eval ExprValue
printLn e = liftIO $ printLnIO e

printLnIO :: ExprValue -> IO ExprValue
printLnIO e = do
    res <- SIO.putStrLn (interpVal e)
    return (NullV)



sleepIOEval :: ExprValue -> Eval ExprValue
sleepIOEval e = liftIO $ sleepIO e

sleepIO :: ExprValue -> IO ExprValue
sleepIO (DoubV n) = do
    let val = (fromIntegral (round n) * 1000000)
    res <- threadDelay val
    return (NullV)
sleepIO (NumV n) = do
    res <- threadDelay (fromIntegral (n * 1000000))
    return (NullV)

wSlurp :: ExprValue -> Eval ExprValue
wSlurp (StringV txt) = liftIO $ openURLhttps txt
wSlurp val = error ("wSlurp expected a string, instead go: " ++ (show val))

openURL :: String -> IO ExprValue
openURL x = do
  req  <- simpleHTTP (getRequest $ x)
  body <- getResponseBody req
  return $ StringV $ body

put :: ExprValue -> ExprValue -> Eval ExprValue
put (StringV path) (StringV msg) = liftIO $ wFilePut path msg
put (StringV _) val = 
    error ("put expects string in the second argument (try using show), instead got : " ++ (show val))
put val _ = error ("put expected string, instead got: " ++ (show val))


openURLhttps :: String -> IO ExprValue
openURLhttps x = do
    res <- (simpleHttp x)
    let body = Char8.unpack res
    return $ StringV $ body



wFilePut :: String -> String -> IO ExprValue
wFilePut fileName msg = withFile fileName WriteMode go
    where go = putTextFile fileName msg

putTextFile :: String -> String -> Handle -> IO ExprValue
putTextFile fileName msg handle = do
    canWrite <- hIsWritable handle
    if canWrite
    then (TIO.hPutStr handle (T.pack msg)) >> (return $ StringV msg)
    else error (" file does not exist: " ++ fileName)



stringOpV :: ExprValue -> String
stringOpV (StringV s) = s
stringOpV e = error ("string op problem: " ++ (show e))

interpVal :: ExprValue -> String
interpVal (NumV n) = show n
interpVal (DoubV n) = show n
interpVal (BoolV b) = boolToString b
interpVal (CharV c) = "#/" ++ (charFormatting c)
interpVal (ListV vals) = "'(" ++ unwords (map interpVal vals) ++ ")"
interpVal (StringV s) = "\"" ++ s ++ "\""
interpVal (ClosureV _ _ _) = "internal function"
interpVal (StructV pairs) = do
    let keys = (map (\x -> (fst x)) pairs)
    let vals = (map (\x -> (interpVal (snd x))) pairs)
    let combined = zip keys vals
    "struct: " ++ "{" ++ unwords (map (\x -> "(" ++ (fst x) ++ " => " ++ (snd x) ++ ")") combined) ++ "}"
interpVal (NullV) = "null"
    
