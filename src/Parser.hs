module Parser where

import DataDefinitions
import Helpers
import Lexer

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


{------------- Parsing -------------}

isInteger s = case reads s :: [(Integer, String)] of
    [(_, "")] -> True
    _         -> False
   
isDouble s = case reads s :: [(Double, String)] of
    [(_, "")] -> True
    _         -> False

isNumeric :: String -> Bool
isNumeric s = isInteger s || isDouble s

isChar :: String -> Bool
isChar s = (take 2 s) == "#/"

checkPieces :: [a] -> Int -> Bool
checkPieces lst n = (length lst) == n

extractSymbol :: LispVal -> String
extractSymbol (Symbol s) = s
extractSymbol e = error ("extract Symbol used incorrectly on: " ++ (show e))

withHelper :: [LispVal] -> WExpr
withHelper ((List ((Symbol s):body:[])):xs:[]) = (WithW s (parser body) (parser xs))
withHelper _ = error "malformed withHelper"

funHelper :: LispVal -> [LispVal]
funHelper (List v) = v
funHelper _ = error "malformed function"

isFunDef :: LispVal -> Bool
isFunDef (List ((Symbol "define"):_)) = True
isFunDef _ = False

getAllFunDefs :: LispVal -> [LispVal]
getAllFunDefs (List x) = (filter isFunDef x)
getAllFunDefs _ = error "getAllFunDefs used incorrectly"

getAllExprs :: LispVal -> [LispVal]
getAllExprs (List x) = (filter (\e -> (not (isFunDef e))) x)
getAllExprs e = error ("error in getAllExprs" ++ (show e))


caseHelper :: [LispVal] -> WExpr
caseHelper lst =
    (CaseW
    (parser (head lst))
    (map (\x -> ((parser (head (unWrapBracket x))), (parser (last (unWrapBracket x))))) (getAllButLast (tail lst)))
    (parser (last (unWrapBracket (last lst)))))

condHelper :: [LispVal] -> WExpr
condHelper lst =
    (CondWT 
    (map (\x -> ((parser (head (unWrapBracket x))), (parser (last (unWrapBracket x))))) (getAllButLast lst)) 
    (parser (last (unWrapBracket (last lst)))))

switchSymbol :: String -> [LispVal] -> WExpr
switchSymbol "+" lv = (AddW (map parser lv)) -- TODO error checking
switchSymbol "-" lv = (SubW (map parser lv))
switchSymbol "*" lv = (MultW (map parser lv))
switchSymbol "/" lv = (DivW (map parser lv))
switchSymbol "=" lv = (EqualW (map parser lv))
switchSymbol "<" lv = (LtW (map parser lv))
switchSymbol ">" lv = (GtW (map parser lv))
switchSymbol "<=" lv = (LtEW (map parser lv))
switchSymbol ">=" lv = (GtEW (map parser lv))
switchSymbol "and" lv = (AndW (map parser lv))
switchSymbol "or" lv = (OrW (map parser lv))
-- switchSymbol "cond" lv = (CondW (parser (lv !! 0)) (parser (lv !! 1)) (parser (lv !! 2)))

switchSymbol "cond" lv = condHelper lv
switchSymbol "case-split" lv = caseHelper lv

switchSymbol "if" lv = (CondW (parser (lv !! 0)) (parser (lv !! 1)) (parser (lv !! 2)))
switchSymbol "lambda" lv = (FunW (map extractSymbol (funHelper (head lv))) (parser (last lv))) -- change to accept multiple arguments
switchSymbol "λ" lv = (FunW (map extractSymbol (funHelper (head lv))) (parser (last lv)))
switchSymbol "with" lv = withHelper lv
switchSymbol "list" lv = (ListW (map parser lv))
switchSymbol "first" lv = (FirstW (parser (head lv)))
switchSymbol "rest" lv = (RestW (parser (head lv)))
switchSymbol "car" lv = (FirstW (parser (head lv)))
switchSymbol "cdr" lv = (RestW (parser (head lv)))
switchSymbol "cons" lv = (ConsW (parser (head lv)) (parser (lv !! 1))) -- expand to be any number
switchSymbol "append" lv = (AppendW (parser (head lv)) (parser (lv !! 1))) -- expand to be any number
switchSymbol "not" lv = (NotW (parser (head lv)))
switchSymbol "empty?" lv = (EmptyW (parser (head lv)))
switchSymbol "slurp!" lv = (SlurpW (parser (head lv)))
switchSymbol "spit!" lv = (SpitW (parser (head lv)) (parser (last lv)))
switchSymbol "string->list" lv = (StringToListW (parser (head lv)))
switchSymbol "list->string" lv = (ListToStringW (parser (head lv)))
switchSymbol "integer?" lv = (IntegerHuhW (parser (head lv)))
switchSymbol "double?" lv = (DoubleHuhW (parser (head lv)))
switchSymbol "string?" lv = (StringHuhW (parser (head lv)))
switchSymbol "char?" lv = (CharHuhW (parser (head lv)))
switchSymbol "list?" lv = (ListHuhW (parser (head lv)))
switchSymbol "closure?" lv = (ClosureHuhW (parser (head lv)))
switchSymbol "bool?" lv = (BoolHuhW (parser (head lv)))
switchSymbol "number?" lv = (NumberHuhW (parser (head lv)))
switchSymbol s lv = (AppW (SymW s) (map parser lv)) -- TODO instead of this, go through the list of deferred subst FIRST then go through the fundefs


appHelper :: [LispVal] -> WExpr
appHelper lv = (AppW (parser (head lv)) (map parser (tail lv)))

isBoolean :: String -> Bool
isBoolean s = (s == "#t") || (s == "#f") || (s == "#true") || (s == "#false") || (s == "#True") || (s == "#False")

unWrapBracket :: LispVal -> [LispVal]
unWrapBracket (ListVal l) = l
unWrapBracket e = error ("unwrapping a bracketed value threw an error: " ++ (show e))

formatChar :: String -> Char
formatChar "#/" = ' '
formatChar s = (last s)

parseSymbol :: String -> WExpr
parseSymbol s =     
    if isInteger s
        then (NumbW (read s::Integer))
        else if isDouble s
            then (DoubW (read s::Double))
            else if (isBoolean s)
                then (BooleanW s)
                else if (isChar s)
                    then (CharW (formatChar s))
                    else (SymW s)

parser :: LispVal -> WExpr
parser (Symbol s) = parseSymbol s
parser (StringVal s) = (StringW s)
parser (List []) = error "Empty expression"
parser (List ((List x):xs)) = appHelper ((List x):xs)
parser (List ((ListVal x):xs)) = (ListW (map parser x)) -- this could be my cond case!
parser (List ((Symbol x):xs)) = switchSymbol x xs
parser (ListVal x) = (ListW (map parser x))
parser _ = error "pattern not matched"

parserWrapper :: [LispVal] -> [WExpr]
parserWrapper lv = (map parser lv)
