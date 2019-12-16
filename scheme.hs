{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scheme where
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
       (integer, float, whiteSpace, stringLiteral, makeTokenParser)
import Text.Parsec.Char (noneOf)
import Text.Parsec.Language (haskell)

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Exception hiding (handle)

-- import Data.Monoid
import System.Directory
import System.IO as SIO
-- import Data.Text.IO as TIO
import Control.Monad.Reader
-- import Network

import Data.Text.IO as TIO
import Data.Text as T hiding (last, unwords, map, tail, head, length, reverse, filter)

-- import Network.HTTP

{------------ Lexing ------------}

-- need a new monad that wraps LispVal with like Eval Expr and IO Expr


data LispVal
  = Symbol String
    | StringVal String
    | List [LispVal]
    | ListVal [LispVal] -- direct translation to lists
    | VectorVal [LispVal]
    deriving (Eq, Show)

tProg :: Prim.ParsecT String a F.Identity LispVal
tProg = tExpr <?> "program"
  where
    tExpr = between ws ws (tListVal <|> tListTick <|> tList <|> tAtom) <?> "expression"
    ws = whiteSpace haskell
    tAtom =
        (StringVal <$> stringLiteral haskell <?> "string") <|>
        (Symbol <$> many1 (noneOf "'()[]\"\t\n\r ") <?> "symbol") <?>
        "atomic expression"
    tList = List <$> between (char '(') (char ')') (many tExpr) <?> "list"
    tListVal = ListVal <$> between (char '[') (char ']') (many tExpr) <?> "list"
    -- tVectorVal = VectorVal <$> between (string "<[") (string "]>") (many tExpr) <?> "vector"
    tListTick = ListVal <$> between (string "'(") (char ')') (many tExpr) <?> "list"
    multiLineCom = ListVal <$> between (string "{-") (string "-}") (many tExpr) <?> "multi-line-comment"

    -- singleLineCom = ListVal <$> between (string "--") (char '\n') (many tExpr) <?> "comment"
    -- singleLineCom2 = ListVal <$> between (char ';') (oneOf "\n\r") (many tExpr) <?> "comment"
    -- endOfFileLine2 = ListVal <$> between (char ';') (eof) (many tExpr) <?> "comment"


unWrap :: LispVal -> [LispVal]
unWrap (List x) = x
unWrap _ = error "unwrap used on a non list"

lexer :: String -> LispVal
lexer s = (fromRight (Symbol "") (parse tProg "" (addParens s)))

addParens :: String -> String
addParens s = "(" ++ s ++ ")"


{------------- Parsing -------------}

isInteger s = case reads s :: [(Integer, String)] of
    [(_, "")] -> True
    _         -> False
   
isDouble s = case reads s :: [(Double, String)] of
    [(_, "")] -> True
    _         -> False

isNumeric :: String -> Bool
isNumeric s = isInteger s || isDouble s

type Symbol = String
type Number = Integer

data WExpr =
    NumbW Integer
    | BooleanW String
    | StringW String
    | SymW [Char]
    | AddW [WExpr]
    | SubW [WExpr]
    | MultW [WExpr]
    | DivW [WExpr]
    | EqualW [WExpr] -- WExpr
    | LtW [WExpr] -- WExpr
    | GtW [WExpr] -- WExpr
    | LtEW [WExpr] -- WExpr
    | GtEW [WExpr] -- WExpr
    | CondW WExpr WExpr WExpr
    | FunW [String] WExpr
    | AppW WExpr [WExpr]
    | WithW String WExpr WExpr
    | ListW [WExpr]
    | AndW [WExpr]
    | OrW [WExpr]
    | FirstW WExpr
    | RestW WExpr
    | ConsW WExpr WExpr
    | AppendW WExpr WExpr
    | NotW WExpr
    | EmptyW WExpr
    | CondWT [(WExpr, WExpr)] WExpr
    | CaseW WExpr [(WExpr, WExpr)] WExpr
    | SlurpW WExpr
    | SpitW WExpr WExpr
    deriving (Eq, Show)

data Expr = 
    Numb Integer
    | StringE String
    | Boolean String
    | Sym [Char]
    | Add Expr Expr
    | Sub Expr Expr
    | Mult Expr Expr
    | Div Expr Expr
    | Equal Expr Expr
    | Lt Expr Expr
    | Gt Expr Expr
    | LtE Expr Expr
    | GtE Expr Expr
    | Cond Expr Expr Expr
    | Fun String Expr
    | App Expr Expr
    | ListE [Expr]
    | And Expr Expr
    | Or Expr Expr
    | First Expr
    | Rest Expr
    | Cons Expr Expr
    | Append Expr Expr
    | Not Expr
    | EmptyE Expr
    | CondT [(Expr, Expr)] Expr
    | Case Expr [(Expr, Expr)] Expr
    | Slurp Expr
    | Spit Expr Expr
    deriving (Eq, Show)


newtype Eval a = Eval { unEval :: ReaderT WExpr IO a }
    deriving (Monad, Functor, Applicative, MonadIO)


slurp :: ExprValue -> Eval ExprValue
slurp (StringV txt) = liftIO $ readTextToStringV txt
slurp val = error ("read expects string, instead got: " ++ (show val))


readTextToStringV :: String -> IO ExprValue
readTextToStringV path = do
    exists <- doesFileExist $ path
    if exists
        then (SIO.readFile path) >>= (return . StringV)
        else error (" file does not exist: " ++ path)

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


-- Replace this with a HashMap
data DefSub = MtSub | ASub Symbol ExprValue DefSub deriving (Eq, Show)

-- Replace every instance of [FunDef] with HashMap
data GlobalFunDef = FundefG String Expr deriving (Eq, Show)

data ExprValue =
    NumV Integer
    | BoolV Bool
    | ClosureV String Expr DefSub
    | ListV [ExprValue]
    | StringV String
    deriving (Eq, Show)

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

parseFunDef :: LispVal -> GlobalFunDef
parseFunDef (List ((Symbol "define") : (List ((Symbol funName) : args) : body : []))) = 
    (FundefG funName (compile (FunW (map extractSymbol args) (parser body))))
parseFunDef _ = error "parseFunDef - malformed function"

parseFunDefs :: [LispVal] -> [GlobalFunDef]
parseFunDefs x = (map parseFunDef x)

getFunDefs :: String -> [GlobalFunDef]
getFunDefs s = (parseFunDefs (getAllFunDefs (lexer s)))


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
switchSymbol s lv = (AppW (SymW s) (map parser lv)) -- TODO instead of this, go through the list of deferred subst FIRST then go through the fundefs


appHelper :: [LispVal] -> WExpr
appHelper lv = (AppW (parser (head lv)) (map parser (tail lv)))

isBoolean :: String -> Bool
isBoolean s = (s == "#t") || (s == "#f") || (s == "#true") || (s == "#false") || (s == "#True") || (s == "#False")

unWrapBracket :: LispVal -> [LispVal]
unWrapBracket (ListVal l) = l
unWrapBracket e = error ("unwrapping a bracketed value threw an error: " ++ (show e))

parser :: LispVal -> WExpr
parser (Symbol s) = 
    if isInteger s
        then (NumbW (read s::Integer))
        else if (isBoolean s)
            then (BooleanW s)
            else (SymW s)
parser (StringVal s) = (StringW s)
parser (List []) = error "Empty expression"
parser (List ((List x):xs)) = appHelper ((List x):xs)
parser (List ((ListVal x):xs)) = (ListW (map parser x)) -- this could be my cond case!
parser (List ((Symbol x):xs)) = switchSymbol x xs
parser (ListVal x) = (ListW (map parser x))
parser _ = error "pattern not matched"

parserWrapper :: [LispVal] -> [WExpr]
parserWrapper lv = (map parser lv)

getAllButLast :: [a] -> [a]
getAllButLast lst = reverse (tail (reverse lst))

-- TODO come back here
leftFoldOptimization :: ([WExpr] -> WExpr) -> (Expr -> Expr -> Expr) -> [WExpr] -> Expr
leftFoldOptimization fnW fn args =
    if (length args == 0)
        then error "Compile - leftFoldOptimization with no arguments"
        else if (length args == 2)
            then (fn (compile (head args)) (compile (last args)))
            else if (length args == 1)
                then (compile (head args))
                -- else (fn (compile (fnW (take 2 args))) (compile (fnW (drop 2 args))))
                else (fn (compile (fnW (getAllButLast args))) (compile (last args)))


boolFoldOptimization :: ([WExpr] -> WExpr) -> (Expr -> Expr -> Expr) -> [WExpr] -> Expr
boolFoldOptimization fnW fn conds =
    if (length conds == 0)
        then error "Compile - bool operation with no arguments"
        else if (length conds == 2)
            then (fn (compile (head conds)) (compile (last conds)))
            else (fn (compile (head conds)) (compile (fnW (tail conds))))


compFoldOptimization :: ([WExpr] -> WExpr) -> (Expr -> Expr -> Expr) -> [WExpr] -> Expr
compFoldOptimization fnW fn args = 
    if (length args == 0)
        then error "compile - comparator with no arguments"
        else if (length args == 2) -- this makes, no sense
            then (fn (compile (head args)) (compile (last args)))
            else if (length args == 1)
                then (Boolean "#t")
                else (And (fn (compile (head args)) (compile (head (tail args)))) (compile (fnW (tail args))))



compile :: WExpr -> Expr
compile (NumbW n) = Numb n
compile (BooleanW b) = Boolean b
compile (SymW s) = Sym s
compile (StringW s) = StringE s
compile (AddW args) = boolFoldOptimization AddW Add args
compile (SubW args) = leftFoldOptimization SubW Sub args
compile (MultW args) = leftFoldOptimization MultW Mult args
compile (DivW args) = leftFoldOptimization DivW Div args
compile (AndW conds) = boolFoldOptimization AndW And conds
compile (OrW conds) = boolFoldOptimization OrW Or conds
compile (NotW cond) = (Not (compile cond))
compile (EqualW args) = compFoldOptimization EqualW Equal args
compile (GtW args) = compFoldOptimization GtW Gt args
compile (LtW args) = compFoldOptimization LtW Lt args
compile (GtEW args) = compFoldOptimization GtEW GtE args
compile (LtEW args) = compFoldOptimization LtEW LtE args
compile (CondW tst thn els) = (Cond (compile tst) (compile thn) (compile els))
compile (WithW name namedExpr body) = (App (Fun name (compile body)) (compile namedExpr))
compile (SlurpW path) = (Slurp (compile path))
compile (SpitW path val) = (Spit (compile path) (compile val))

compile (AppW funExpr argExprs) =
    if (length argExprs == 0)
        then error ("Compile - Nullary Application: " ++ (show funExpr)) -- TODO make it so you can use no arguments!
        -- then (App )
        else if (length argExprs == 1)
            then (App (compile funExpr) (compile (head argExprs)))
            else (App (compile (AppW funExpr (getAllButLast argExprs))) (compile (last argExprs)))

compile (FunW paramNames body) =
    if (length paramNames) == 0
        then error "Compile - Nullary Function" -- TODO make it so you can use no arguments!
        else if (length paramNames == 1)
            then (Fun (head paramNames) (compile body))
            else (Fun (head paramNames) (compile (FunW (tail paramNames) body)))

compile (FirstW lst) = (First (compile lst))
compile (RestW lst) = (Rest (compile lst))
compile (ListW vals) = (ListE (map compile vals))
compile (ConsW l r) = (Cons (compile l) (compile r))
compile (AppendW l r) = (Append (compile l) (compile r))
compile (EmptyW lst) = (EmptyE (compile lst))
compile (CondWT tests els) =
    (CondT (map (\x -> ((compile (fst x)), (compile (snd x)))) tests) (compile els))
compile (CaseW exp cases els) =
    (Case (compile exp) (map (\x -> ((compile (fst x)), (compile (snd x)))) cases) (compile els))

compileMap :: [WExpr] -> [Expr]
compileMap wEs = (map compile wEs)

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
numOpAdd _ _ = (error "Wrong value given to addition")

numOpSub :: ExprValue -> ExprValue -> ExprValue
numOpSub (NumV l) (NumV r) = (NumV (l - r))
numOpSub _ _ = (error "Wrong value given to addition")

numOpMult :: ExprValue -> ExprValue -> ExprValue
numOpMult (NumV l) (NumV r) = (NumV (l * r))
numOpMult _ _ = (error "Wrong value given to addition")

numOpDiv :: ExprValue -> ExprValue -> ExprValue
numOpDiv (NumV l) (NumV r) = (NumV (l `div` r))
numOpDiv _ _ = (error "Wrong value given to addition")

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

extractValue :: ExprValue -> Expr
extractValue (ListV lst) = (ListE (map extractValue lst))
extractValue (NumV n) = (Numb n)
extractValue (BoolV b) = (Boolean (boolToString b))
extractValue (ClosureV paramName body ds) = (Fun paramName body)

boolOp :: ExprValue -> Bool
boolOp (BoolV b) = b
boolOp _ = error "bool operation applied to non bool"

stringOp :: ExprValue -> String
stringOp (StringV s) = s
stringOp e = error ("String operation applied to non string: " ++ (show e))

stringToExpr :: ExprValue -> Eval Expr
stringToExpr (StringV s) = return (StringE s)
stringToExpr e = error ("String operation applied to non string: " ++ (show e))

-- testInterp :: (Eval Expr) -> [GlobalFunDef] -> DefSub -> ExprValue
-- testInterp (Eval (StringE s)) _ _ = StringV s

-- data IFunc = IFunc { fn :: [LispVal] -> Eval LispVal }
--     deriving (Typeable)

-- mkF :: ([LispVal] -> Eval LispVal) -> LispVal
-- mkF = Fun . IFunc

-- interpEval :: Expr -> [GlobalFunDef] -> DefSub -> Eval ExprValue
-- interpEval (Slurp s) funDefs ds = interpExpr 

-- interpEval expr funDefs ds = return (interp expr funDefs ds)

-- interpExpr :: Expr -> [GlobalFunDef] -> DefSub -> Eval Expr
-- interpExpr (Slurp s) funDefs ds = stringToExpr (interp (Slurp s) funDefs ds)


    -- do
    --     let res = stringToExpr >=> (interp s funDefs ds)
    --     interp res funDefs ds



-- interpSlurp :: Expr -> [GlobalFunDef] -> DefSub -> Eval ExprValue
-- interpSlurp expr funDefs ds = return (interp expr funDefs ds)

-- TODO come back here
interp :: Expr -> [GlobalFunDef] -> DefSub -> Eval ExprValue
interp (Numb n) _ _ = return (NumV n)
interp (Boolean b) _ _ = return (BoolV (matchStrToBool b))
interp (Sym s) funDefs ds = do 
    res <- (lookupDS (Symbol s) funDefs ds)
    return res

interp (StringE s) _ _ = return (StringV s)
-- interp (Slurp s) funDefs ds = 
--     do
--         (interpEval (Slurp s) funDefs ds)


interp (Add lhs rhs) funDefs ds = do
    l <- interp lhs funDefs ds
    r <- interp rhs funDefs ds
    return (numOpAdd l r)

    -- res <- numOpAdd (interp lhs funDefs ds) (interp rhs funDefs ds)
    -- return res
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
    return (BoolV ((checkNumber l) < (checkNumber r)))

interp (Gt lhs rhs) funDefs ds = do 
    l <- interp lhs funDefs ds
    r <- interp rhs funDefs ds
    return (BoolV ((checkNumber l) > (checkNumber r)))

interp (LtE lhs rhs) funDefs ds = do 
    l <- interp lhs funDefs ds
    r <- interp rhs funDefs ds
    return (BoolV ((checkNumber l) <= (checkNumber r)))

interp (GtE lhs rhs) funDefs ds = do 
    l <- interp lhs funDefs ds
    r <- interp rhs funDefs ds
    return (BoolV ((checkNumber l) >= (checkNumber r)))

    -- BoolV ((checkNumber (interp lhs funDefs ds)) < (checkNumber (interp rhs funDefs ds)))
-- interp (Gt lhs rhs) funDefs ds = BoolV ((checkNumber (interp lhs funDefs ds)) > (checkNumber (interp rhs funDefs ds)))
-- interp (LtE lhs rhs) funDefs ds = BoolV ((checkNumber (interp lhs funDefs ds)) <= (checkNumber (interp rhs funDefs ds)))
-- interp (GtE lhs rhs) funDefs ds = BoolV ((checkNumber (interp lhs funDefs ds)) >= (checkNumber (interp rhs funDefs ds)))
interp (App funExpr argExpr) funDefs ds = do
    fn <- (interp funExpr funDefs ds)
    ag <- (interp argExpr funDefs ds)
    appEval fn ag funDefs

interp (Fun paramName body) _ ds = return (ClosureV paramName body ds) -- paramName has to be some or none

interp (Cond test thn els) funDefs ds = -- expand to any number of conditions
    do
        tst <- (interp test funDefs ds)
        if (evalTestBool tst)
            then (interp thn funDefs ds)
            else (interp els funDefs ds)

interp (CondT tests els) funDefs ds =
    -- tst <- (interp (fst (head tests)) funDefs ds)
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

interp (ListE vals) funDefs ds = do
    res <- (mapM (\x -> (interp x funDefs ds)) vals)
    return (ListV res)

    -- return (ListV (mapM (map (\x -> (interp x funDefs ds)) vals)))



interp (First lst) funDefs ds = do 
    res <- (interp lst funDefs ds)
    return (head (listOpV res))
    
    -- (head (listOpV (interp lst funDefs ds)))-- need to split on the exprvalue cases for the result
-- interp (First lst) funDefs ds = (interp (head (listOpV lst)) funDefs ds)
interp (Rest lst) funDefs ds = do
    res <- (interp lst funDefs ds)
    return (ListV (tail (listOpV res)))
    
    -- (ListV (tail (listOpV (interp lst funDefs ds))))
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
    return (BoolV (length (listOpV res) == 0))
interp (And lhs rhs) funDefs ds =  do -- TODO check if this is actually a necessary optimization?
    l <- (interp lhs funDefs ds)
    if l == (BoolV True)
        then (interp rhs funDefs ds)
        else return (BoolV False)
        -- else if (interp rhs funDefs ds) == (BoolV True)
        --     then (BoolV True)
        --     else (BoolV False)
interp (Or lhs rhs) funDefs ds = do
    l <- (interp lhs funDefs ds)
    if l == (BoolV True)
        then return (BoolV True)
        else do 
            r <- (interp rhs funDefs ds)
            if r == (BoolV True)
            then return (BoolV True)
            else return (BoolV False)

-- TODO pick up here
interpWrap :: Expr -> [GlobalFunDef] -> Eval ExprValue
interpWrap s funDefs = interp s funDefs (MtSub)

multipleInterp :: [Expr] -> [GlobalFunDef] -> Eval [ExprValue]
multipleInterp s funDefs = (mapM (\x -> (interpWrap x funDefs)) s)


interpVal :: ExprValue -> String
interpVal (NumV n) = show n
interpVal (BoolV b) = boolToString b
interpVal (ListV vals) = "'(" ++ unwords (map interpVal vals) ++ ")"
interpVal (StringV s) = "\"" ++ s ++ "\""
interpVal (ClosureV _ _ _) = "internal function"


multipleInterpVal :: [ExprValue] -> [String]
multipleInterpVal evs = (map interpVal evs)

multipleInterpValL :: Eval [ExprValue] -> Eval [String]
multipleInterpValL = liftM multipleInterpVal


{-
Somehow, inject the string from the path 
into the file in some capacity at run time
(ideally, lazily in some way but idk if I can pull that off)
-}
-- slurp :: ExprValue -> IO String
-- slurp (StringV s) = readFile s
-- slurp e = error ("Not a valid path" ++ (show e))



-- eval :: String -> [String]
-- eval expr =
--     (multipleInterpVal 
--     (multipleInterp
--     (compileMap (parserWrapper (getAllExprs (lexer expr)))) 
--     (getFunDefs expr)))

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

-- evalWithStdLib :: String -> String -> [String]
-- evalWithStdLib expr file =
--     (multipleInterpVal
--     (multipleInterp
--     (compileMap (parserWrapper (getAllExprs (lexer expr))))
--     ((getFunDefs file) ++ (getFunDefs expr))))



-- main :: IO ()
-- main = do

--     -- let expr = "(cond [(= 1 1) '(1 2 3 4)] [(= 3 4) (list 4 5 6)] [else (list 7 8 9)])"

--     -- let expr = "(#import stdlib.scm) (#import helpers.scm)"

--     -- let expr = "\"hello\""

--     let expr = "(slurp \"fac.scm\")"

--     -- let res = (multipleInterp (compileMap (parserWrapper (getAllExprs (lexer expr)))) (getFunDefs expr))

--     test1 <- eval expr

--     print (test1)

    -- let expr2 = (StringV "fac.scm")

    -- let res2 = (slurp expr2)

    -- -- print (res)

    -- -- let undo = runReaderT (unEval res) (StringW "")

    -- -- print (undo)

    -- -- unEval res

    -- test <- runReaderT (unEval res2) (StringW "")

    -- print (test)

    -- let expr2 = "((lambda () (+ 5 5)))"

--     print (parserWrapper (getAllExprs (lexer expr)))
--     print (parserWrapper (getAllExprs (lexer expr2)))

--     -- print (eval expr)

    -- print (compileMap (parserWrapper (getAllExprs (lexer expr))))

    -- print (lexer expr)

    -- expr <- (readFile "fac.scm")



{-
NOTES:
(define (my-func arg1 arg2)
    (+ arg1 arg2))

(my-func arg1 arg2) -> application w/ arg1 arg2
(begin - number of statements)

=> (with my-func (fun (arg1 arg2) (+ arg1 arg2)))

"begin" support
implicit 'begin' to start a file
support the syntax that I want
support custom syntax
support multiplication and division
support different number types
support recursion!
support for loops and while loops (via the compiler?)
support lambda for fun!
support string
-}
