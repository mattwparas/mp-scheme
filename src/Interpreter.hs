module Interpreter where

import DataDefinitions
import Helpers
import Lexer
import Compiler
import Parser

import Data.Typeable (Typeable)
import Data.Tree
import Text.Parsec
import Text.Parsec.String
import Data.Either
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Functor.Identity as F
import qualified Text.Parsec.Prim as Prim
import Text.Parsec
        ((<|>), (<?>), many, many1, char, try, parse, sepBy, choice,
        between)
import Text.Parsec.Token
        (integer, float, whiteSpace, stringLiteral, makeTokenParser, charLiteral)
import Text.Parsec.Char (noneOf)
import Text.Parsec.Language (haskell)

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Exception hiding (handle, try)

-- import Data.Monoid
import System.Directory
import System.IO as SIO
-- import Data.Text.IO as TIO
import Control.Monad.Reader
-- import Network

import Data.Text.IO as TIO
import Data.Text as T hiding (last, unwords, map, tail, head, length, reverse, filter, try, take)


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




-- TODO come back here

-- wSlurp :: ExprValue -> Eval ExprValue
-- wSlurp (StringV txt) = liftIO $ openURL txt
-- wSlurp val = error ("wSlurp expected a string, instead go: " ++ (show val))

-- openURL :: String -> IO LispVal
-- openURL x = do
--   req  <- simpleHTTP (getRequest $ x)
--   body <- getResponseBody req
--   return $ String $ body



put :: ExprValue -> ExprValue -> Eval ExprValue
put (StringV path) (StringV msg) = liftIO $ wFilePut path msg
put (StringV _) val = 
    error ("put expects string in the second argument (try using show), instead got : " ++ (show val))
put val _ = error ("put expected string, instead got: " ++ (show val))


wFilePut :: String -> String -> IO ExprValue
wFilePut fileName msg = withFile fileName WriteMode go
    where go = putTextFile fileName msg

putTextFile :: String -> String -> Handle -> IO ExprValue
putTextFile fileName msg handle = do
    canWrite <- hIsWritable handle
    if canWrite
    then (TIO.hPutStr handle (T.pack msg)) >> (return $ StringV msg)
    else error (" file does not exist: " ++ fileName)
    

-- Lookup symbol to see if in DS, if not check global function definitions
lookupDS :: LispVal -> [GlobalFunDef] -> DefSub -> Eval ExprValue
lookupDS (Symbol s1) funDefs (MtSub) = interp (lookupFundefs s1 funDefs) funDefs (MtSub)
lookupDS (Symbol s1) funDefs (ASub s2 val rest) = 
    if s1 == s2
        then return val
        else (lookupDS (Symbol s1) funDefs rest)
lookupDS _ _ _ = error "lookupDS malformed"

lookupFundefs :: String -> [GlobalFunDef] -> Expr
lookupFundefs s [] = error ("interp - free identifier" ++ s)
lookupFundefs s ((FundefG funName closure):xs) = 
    if funName == s
        then closure
        else (lookupFundefs s xs)

-- TODO abstract numOp to any operator, use generically in place of all of these
-- TRY TO FIX THIS
-- numOp :: ExprValue -> ExprValue -> (Integer -> Integer -> Integer) -> ExprValue
-- numOP (NumV l) (NumV r) fn = (NumV ((fn) l r))
-- numOp _ _ _ = (error "Wrong value given to numerical operator")

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
numOpCompGt _ _ = (error "Wrong value given to less than")

numOpCompLtE :: ExprValue -> ExprValue -> ExprValue
numOpCompLtE (NumV l) (NumV r) = (BoolV (l <= r))
numOpCompLtE (DoubV l) (DoubV r) = (BoolV (l <= r))
numOpCompLtE (NumV l) (DoubV r) = (BoolV ((fromIntegral l) <= r))
numOpCompLtE (DoubV l) (NumV r) = (BoolV (l <= (fromIntegral r)))
numOpCompLtE _ _ = (error "Wrong value given to less than")

numOpCompGtE :: ExprValue -> ExprValue -> ExprValue
numOpCompGtE (NumV l) (NumV r) = (BoolV (l >= r))
numOpCompGtE (DoubV l) (DoubV r) = (BoolV (l >= r))
numOpCompGtE (NumV l) (DoubV r) = (BoolV ((fromIntegral l) >= r))
numOpCompGtE (DoubV l) (NumV r) = (BoolV (l >= (fromIntegral r)))
numOpCompGtE _ _ = (error "Wrong value given to less than")



evalTestBool :: ExprValue -> Bool
evalTestBool (BoolV b) = b
evalTestBool _ = error "Invalid bool Value"

-- TODO get rid of this?
evalEquality :: ExprValue -> ExprValue -> ExprValue
evalEquality (NumV l) (NumV r) = BoolV (l == r)
evalEquality (BoolV l) (BoolV r) = BoolV (l == r)
evalEquality _ _ = BoolV False

appEval :: ExprValue -> ExprValue -> [GlobalFunDef] -> Eval ExprValue
-- appEval (ClosureV paramName body ds)  funDefs = (interp body funDefs ds) -- make it empty list
appEval (ClosureV paramName body ds) argVal funDefs = (interp body funDefs (ASub paramName argVal ds))
appEval _ _ _ = error "expected function"

appEvalNoArgs :: ExprValue -> [GlobalFunDef] -> Eval ExprValue
-- appEval (ClosureV paramName body ds)  funDefs = (interp body funDefs ds) -- make it empty list
appEvalNoArgs (ClosureV paramName body ds) funDefs = (interp body funDefs ds)
appEvalNoArgs _ _ = error "expected function"


matchStrToBool :: String -> Bool
matchStrToBool "#t" = True
matchStrToBool "#f" = False
matchStrToBool "#true" = True
matchStrToBool "#false" = False
matchStrToBool "#T" = True
matchStrToBool "#F" = False
matchStrToBool "#True" = True
matchStrToBool "#False" = False
matchStrToBool _ = error "Boolean malformed"

defSubHelper :: [String] -> [GlobalFunDef] -> [Expr] -> DefSub -> Eval DefSub
defSubHelper [] funDefs [] ds = return (MtSub)
defSubHelper [] funDefs (x:xs) ds = error "interp: wrong arity"
defSubHelper (x:xs) funDefs [] ds = error "interp: wrong arity"
defSubHelper (x:xs) funDefs (b:bs) ds = 
    do 
        res <- (interp b funDefs ds)
        defs <- (defSubHelper xs funDefs bs ds)
        return (ASub x res defs)

checkNumber :: ExprValue -> Number
checkNumber (NumV n) = n
checkNumber e = error ("comparator not supported for non numbers: " ++ (show e))

listOpV :: ExprValue -> [ExprValue]
listOpV (ListV lst) = lst
listOpV e = error ("List operation applied to non list: " ++ (show e))

boolToString :: Bool -> String
boolToString True = "#t"
boolToString False = "#f"

boolOp :: ExprValue -> Bool
boolOp (BoolV b) = b
boolOp _ = error "bool operation applied to non bool"

stringOp :: ExprValue -> String
stringOp (StringV s) = s
stringOp e = error ("String operation applied to non string: " ++ (show e))

stringToExpr :: ExprValue -> Eval Expr
stringToExpr (StringV s) = return (StringE s)
stringToExpr e = error ("String operation applied to non string: " ++ (show e))

listOpE :: Expr -> [Expr]
listOpE (ListE lst) = lst
listOpE e = error ("List operation applied to non list: " ++ (show e))

charOp :: ExprValue -> Char
charOp (CharV c) = c
charOp e = error ("Char operation applied to non char: " ++ (show e))

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



-- TODO come back here
interp :: Expr -> [GlobalFunDef] -> DefSub -> Eval ExprValue
interp (Numb n) _ _ = return (NumV n)
interp (Doub n) _ _ = return (DoubV n)
interp (Boolean b) _ _ = return (BoolV (matchStrToBool b))
interp (CharE c) _ _ = return (CharV c)
interp (Sym s) funDefs ds = do 
    res <- (lookupDS (Symbol s) funDefs ds)
    return res

interp (StringE s) _ _ = return (StringV s)

interp (Add lhs rhs) funDefs ds = do
    l <- interp lhs funDefs ds
    r <- interp rhs funDefs ds
    return (numOpAdd l r)

interp (Sub lhs rhs) funDefs ds = do
    l <- interp lhs funDefs ds
    r <- interp rhs funDefs ds
    return (numOpSub l r)

interp (Mult lhs rhs) funDefs ds = do
    l <- interp lhs funDefs ds
    r <- interp rhs funDefs ds
    return (numOpMult l r)

interp (Div lhs rhs) funDefs ds = do
    l <- interp lhs funDefs ds
    r <- interp rhs funDefs ds
    return (numOpDiv l r)

interp (Equal lhs rhs) funDefs ds = do
    l <- interp lhs funDefs ds
    r <- interp rhs funDefs ds
    return (BoolV (l == r))

interp (Lt lhs rhs) funDefs ds = do 
    l <- interp lhs funDefs ds
    r <- interp rhs funDefs ds
    return (numOpCompLt l r)

interp (Gt lhs rhs) funDefs ds = do 
    l <- interp lhs funDefs ds
    r <- interp rhs funDefs ds
    return (numOpCompGt l r)

interp (LtE lhs rhs) funDefs ds = do 
    l <- interp lhs funDefs ds
    r <- interp rhs funDefs ds
    return (numOpCompLtE l r)

interp (GtE lhs rhs) funDefs ds = do 
    l <- interp lhs funDefs ds
    r <- interp rhs funDefs ds
    return (numOpCompGtE l r)

interp (App funExpr (argExpr:xs)) funDefs ds = do
    fn <- (interp funExpr funDefs ds)
    ag <- (interp argExpr funDefs ds)
    appEval fn ag funDefs

interp (App funExpr []) funDefs ds = do
    fn <- (interp funExpr funDefs ds)
    appEvalNoArgs fn funDefs

interp (Fun paramName body) _ ds = 
    if paramName == []
        then return (ClosureV "" body ds) -- paramName has to be some or none
        else return (ClosureV (head paramName) body ds)

interp (Cond test thn els) funDefs ds = -- expand to any number of conditions
    do
        tst <- (interp test funDefs ds)
        if (evalTestBool tst)
            then (interp thn funDefs ds)
            else (interp els funDefs ds)

interp (CondT tests els) funDefs ds =
    if tests == []
        then (interp els funDefs ds)
        else do
            tst <- (interp (fst (head tests)) funDefs ds)
            if (evalTestBool tst)
            then (interp (snd (head tests)) funDefs ds)
            else (interp (CondT (tail tests) els) funDefs ds)

interp (Case expr conds els) funDefs ds = -- TODO optimize this if possible
    if conds == []
        then (interp els funDefs ds)
        else do
            l <- (interp (fst (head conds)) funDefs ds)
            r <- (interp expr funDefs ds)
            if l == r
            then (interp (snd (head conds)) funDefs ds)
            else (interp (Case expr (tail conds) els) funDefs ds)

interp (Slurp s) funDefs ds = do
    res <- (interp s funDefs ds)
    (slurp res)

interp (Spit path res) funDefs ds = do
    filePath <- (interp path funDefs ds)
    msg <- (interp res funDefs ds)
    (put filePath msg)

interp (StringToList str) funDefs ds = do
    res <- (interp str funDefs ds)
    let intstr = stringOp res
    return (ListV (map (\x -> (CharV x)) intstr))

interp (ListToString lst) funDefs ds = do
    res <- (interp lst funDefs ds)
    let intlst = listOpV res
    return (StringV (map charOp intlst))

interp (ListE vals) funDefs ds = do
    res <- (mapM (\x -> (interp x funDefs ds)) vals)
    return (ListV res)

interp (First lst) funDefs ds = do
    res <- (interp lst funDefs ds)
    return (firstOp res)
    -- return (head (listOpV res))

interp (Rest lst) funDefs ds = do
    res <- (interp lst funDefs ds)
    return (restOp res)
    -- return (ListV (tail (listOpV res)))

interp (Cons l r) funDefs ds = do
    hd <- (interp l funDefs ds)
    tl <- (interp r funDefs ds)
    return (ListV (hd : (listOpV tl))) -- TODO expand to any number of arguments
    
interp (Append l r) funDefs ds = do
    ll <- (interp l funDefs ds)
    rr <- (interp r funDefs ds)
    return (ListV ((listOpV ll) ++ (listOpV rr)))

interp (Not cond) funDefs ds = do
    res <- (interp cond funDefs ds)
    return (BoolV (not (boolOp res)))
interp (EmptyE lst) funDefs ds = do
    res <- (interp lst funDefs ds)
    return (emptyOp res)
    -- return (BoolV (length (listOpV res) == 0))
interp (And lhs rhs) funDefs ds =  do -- TODO check if this is actually a necessary optimization?
    l <- (interp lhs funDefs ds)
    if l == (BoolV True)
        then (interp rhs funDefs ds)
        else return (BoolV False)

interp (Or lhs rhs) funDefs ds = do
    l <- (interp lhs funDefs ds)
    if l == (BoolV True)
        then return (BoolV True)
        else do 
            r <- (interp rhs funDefs ds)
            if r == (BoolV True)
            then return (BoolV True)
            else return (BoolV False)

interp (IntegerHuh int) funDefs ds = do
    res <- (interp int funDefs ds)
    return (integerHuh res)

interp (DoubleHuh dub) funDefs ds = do
    res <- (interp dub funDefs ds)
    return (doubleHuh res)

interp (ClosureHuh fun) funDefs ds = do
    res <- (interp fun funDefs ds)
    return (closureHuh res)

interp (ListHuh lst) funDefs ds = do
    res <- (interp lst funDefs ds)
    return (listHuh res)

interp (StringHuh str) funDefs ds = do
    res <- (interp str funDefs ds)
    return (stringHuh res)

interp (CharHuh char) funDefs ds = do
    res <- (interp char funDefs ds)
    return (charHuh res)

interp (BoolHuh b) funDefs ds = do
    res <- (interp b funDefs ds)
    return (boolHuh res)

interp (NumberHuh n) funDefs ds = do
    res <- (interp n funDefs ds)
    return (numberHuh res)

interp (UserInput) funDefs ds = do
    userInput

interp (PrintLn expr) funDefs ds = do
    res <- (interp expr funDefs ds)
    printLn res

integerHuh :: ExprValue -> ExprValue
integerHuh (NumV _) = BoolV True
integerHuh _ = BoolV False

doubleHuh :: ExprValue -> ExprValue
doubleHuh (DoubV _) = BoolV True
doubleHuh _ = BoolV False

closureHuh :: ExprValue -> ExprValue
closureHuh (ClosureV _ _ _) = BoolV True
closureHuh _ = BoolV False

listHuh :: ExprValue -> ExprValue
listHuh (ListV _) = BoolV True
listHuh _ = BoolV False

stringHuh :: ExprValue -> ExprValue
stringHuh (StringV _) = BoolV True
stringHuh _ = BoolV False

charHuh :: ExprValue -> ExprValue
charHuh (CharV _) = BoolV True
charHuh _ = BoolV False

numberHuh :: ExprValue -> ExprValue
numberHuh (DoubV _) = BoolV True
numberHuh (NumV _) = BoolV True
numberHuh _ = BoolV False

boolHuh :: ExprValue -> ExprValue
boolHuh (BoolV _) = BoolV True
boolHuh _ = BoolV False


parseFunDef :: LispVal -> GlobalFunDef
parseFunDef (List ((Symbol "define") : (List ((Symbol funName) : args) : body : []))) = 
    (FundefG funName (compile (FunW (map extractSymbol args) (parser body))))
parseFunDef _ = error "parseFunDef - malformed function"

parseFunDefs :: [LispVal] -> [GlobalFunDef]
parseFunDefs x = (map parseFunDef x)

getFunDefs :: String -> [GlobalFunDef]
getFunDefs s = (parseFunDefs (getAllFunDefs (lexer s)))

            -- TODO pick up here
interpWrap :: Expr -> [GlobalFunDef] -> Eval ExprValue
interpWrap s funDefs = interp s funDefs (MtSub)

multipleInterp :: [Expr] -> [GlobalFunDef] -> Eval [ExprValue]
multipleInterp s funDefs = (mapM (\x -> (interpWrap x funDefs)) s)

charFormatting :: Char -> String
charFormatting ' ' = "space"
charFormatting c = [c]

interpVal :: ExprValue -> String
interpVal (NumV n) = show n
interpVal (DoubV n) = show n
interpVal (BoolV b) = boolToString b
interpVal (CharV c) = "#/" ++ (charFormatting c)
interpVal (ListV vals) = "'(" ++ unwords (map interpVal vals) ++ ")"
interpVal (StringV s) = "\"" ++ s ++ "\""
interpVal (ClosureV _ _ _) = "internal function"
interpVal (NullV) = "null"


multipleInterpVal :: [ExprValue] -> [String]
multipleInterpVal evs = (map interpVal evs)

multipleInterpValL :: Eval [ExprValue] -> Eval [String]
multipleInterpValL = liftM multipleInterpVal

eval :: String -> IO [String]
eval expr = do
    let res = (multipleInterp (compileMap (parserWrapper (getAllExprs (lexer expr)))) (getFunDefs expr))
    (runReaderT (unEval (multipleInterpValL res)) (StringW ""))

    -- (map (\x -> (runReaderT (unEval x) (StringW ""))) test)

evalWithStdLib :: String -> String -> IO [String]
evalWithStdLib expr file = do
    let res = 
            (multipleInterp 
            (compileMap (parserWrapper (getAllExprs (lexer expr))))
            ((getFunDefs file) ++ (getFunDefs expr)))
    (runReaderT (unEval (multipleInterpValL res)) (StringW ""))
