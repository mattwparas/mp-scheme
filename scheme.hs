-- {-# LANGUAGE MultiWayIf #-}

module Scheme where

import Data.Tree
import Text.Parsec
import Text.Parsec.String
import Data.Either
-- import Data.HashMap.Lazy
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

{------------ Lexing ------------}

data LispVal
  = Symbol String
    | List [LispVal]
    deriving (Eq, Show)

tProg :: Prim.ParsecT String a F.Identity LispVal
tProg = tExpr <?> "program"
  where
    tExpr = between ws ws (tList <|> tAtom) <?> "expression"
    ws = whiteSpace haskell
    tAtom =
        -- (try (Float <$> float haskell) <?> "floating point number") <|>
        -- (try (Int <$> integer haskell) <?> "integer") <|>
        -- (String <$> stringLiteral haskell <?> "string") <|>
        -- (Symbol <$> many1 (noneOf "()\"\t\n\r ") <?> "symbol") <?>
        -- (String <$> stringLiteral haskell <?> "string") <|>
        -- (try (Int <$> integer haskell) <?> "integer") <|>
        (Symbol <$> many1 (noneOf "()\"\t\n\r ") <?> "symbol") <?>
        "atomic expression"
    tList = List <$> between (char '(') (char ')') (many tExpr) <?> "list"


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
    -- | FunAppW String [WExpr]
    | ListW [WExpr]
    | AndW [WExpr]
    | OrW [WExpr]
    | FirstW WExpr
    | RestW WExpr
    | ConsW WExpr WExpr
    | AppendW WExpr WExpr
    | NotW WExpr
    | EmptyW WExpr
    -- | MapW WExpr WExpr
    -- | FilterW WExpr WExpr
    -- | BeginW [WExpr]
    -- | Cond [(Expr, Expr)] Expr
    deriving (Eq, Show)

data Expr = 
    Numb Integer
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
    -- | FunApp Expr [Expr]
    | ListE [Expr]
    | And Expr Expr
    | Or Expr Expr
    | First Expr
    | Rest Expr
    | Cons Expr Expr
    | Append Expr Expr
    | Not Expr
    | EmptyE Expr
    -- | MapE Expr Expr
    -- | FilterE Expr Expr
    -- | Begin Expr Expr
    -- | With String Expr Expr
    -- | Cond [(Expr, Expr)] Expr
    deriving (Eq, Show)

{-
(cond ((expression) (then)) (else))
-}

-- Replace this with a HashMap
data DefSub = MtSub | ASub Symbol ExprValue DefSub deriving (Eq, Show)

-- Replace every instance of [FunDef] with HashMap
data FunDef = Fundef String [String] Expr deriving (Eq, Show)

data GlobalFunDef = FundefG String Expr deriving (Eq, Show)

data ExprValue =
    NumV Integer
    | BoolV Bool
    | ClosureV String Expr DefSub
    | ListV [ExprValue]
    deriving (Eq, Show)


checkPieces :: [a] -> Int -> Bool
checkPieces lst n = (length lst) == n

extractSymbol :: LispVal -> String
extractSymbol (Symbol s) = s
extractSymbol _ = error "extract Symbol used incorrectly"

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
getAllExprs _ = error "idk"


-- parseFunDef :: LispVal -> FunDef
-- parseFunDef (List ((Symbol "define") : (List ((Symbol funName) : args) : body : []))) = 
--     (Fundef funName (map extractSymbol args) (compile (parser body)))
-- parseFunDef _ = error "parseFunDef - malformed function"

-- parseFunDefs :: [LispVal] -> [FunDef]
-- parseFunDefs x = (map parseFunDef x)

-- getFunDefs :: String -> [FunDef]
-- getFunDefs s = (parseFunDefs (getAllFunDefs (lexer s)))


parseFunDef :: LispVal -> GlobalFunDef
parseFunDef (List ((Symbol "define") : (List ((Symbol funName) : args) : body : []))) = 
    (FundefG funName (compile (FunW (map extractSymbol args) (parser body))))
parseFunDef _ = error "parseFunDef - malformed function"

parseFunDefs :: [LispVal] -> [GlobalFunDef]
parseFunDefs x = (map parseFunDef x)

getFunDefs :: String -> [GlobalFunDef]
getFunDefs s = (parseFunDefs (getAllFunDefs (lexer s)))


{-

(FundefG funName (compile (FunW (map extractSymbol args) (compile parser body))))

-}


-- TODO come back here pls
-- callGlobalFunc :: String -> [LispVal] -> WExpr
-- callGlobalFunc s lv = (FunAppW s (map parser lv))


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
switchSymbol "cond" lv = (CondW (parser (lv !! 0)) (parser (lv !! 1)) (parser (lv !! 2)))
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
-- switchSymbol "map" lv = (MapW (parser (head lv)) (parser (last lv)))
-- switchSymbol "filter" lv = (FilterW (parser (head lv)) (parser (last lv)))
-- switchSymbol "begin" lv = (BeginW (map parser lv))
switchSymbol s lv = (AppW (SymW s) (map parser lv)) -- TODO instead of this, go through the list of deferred subst FIRST then go through the fundefs
-- make fundefs follow the same patterns

-- switchSymbol _ lv = (AppW (parser (lv !! 0)) (map parser (tail lv)))
-- switchSymbol _ lv = appHelper lv

appHelper :: [LispVal] -> WExpr
appHelper lv = (AppW (parser (head lv)) (map parser (tail lv)))

isBoolean :: String -> Bool
isBoolean s = (s == "#t") || (s == "#f")

parser :: LispVal -> WExpr
parser (Symbol s) = 
    if isInteger s
        then (NumbW (read s::Integer))
        else if (isBoolean s)
            then (BooleanW s)
            else (SymW s)
parser (List []) = error "Empty expression"
parser (List ((List x):xs)) = appHelper ((List x):xs) -- global function calls are going here, which they shouldnt be
parser (List ((Symbol x):xs)) = switchSymbol x xs

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
                else (fn (compile (fnW (take 2 args))) (compile (fnW (drop 2 args))))

boolFoldOptimization :: ([WExpr] -> WExpr) -> (Expr -> Expr -> Expr) -> [WExpr] -> Expr
boolFoldOptimization fnW fn conds =
    if (length conds == 0)
        then error "Compile - bool operation with no arguments"
        else if (length conds == 2)
            then (fn (compile (head conds)) (compile (last conds)))
            else (fn (compile (fnW (take 2 conds))) (compile (fnW (drop 2 conds))))

compFoldOptimization :: ([WExpr] -> WExpr) -> (Expr -> Expr -> Expr) -> [WExpr] -> Expr
compFoldOptimization fnW fn args = 
    if (length args == 0)
        then error "compile - comparator with no arguments"
        else if (length args == 1)
            then (fn (compile (head args)) (compile (last args)))
            else (And (fn (compile (head args)) (compile (head (tail args)))) (compile (fnW (tail args))))


compile :: WExpr -> Expr
compile (NumbW n) = Numb n
compile (BooleanW b) = Boolean b
compile (SymW s) = Sym s
compile (AddW args) = leftFoldOptimization AddW Add args
compile (SubW args) = leftFoldOptimization SubW Sub args
compile (MultW args) = leftFoldOptimization MultW Mult args
compile (DivW args) = leftFoldOptimization DivW Div args
compile (AndW conds) = boolFoldOptimization AndW And conds
compile (OrW conds) = boolFoldOptimization OrW Or conds
compile (NotW cond) = (Not (compile cond))
compile (EqualW args) = compFoldOptimization EqualW Equal args
compile (GtW args) = compFoldOptimization GtW Gt args -- (Gt (compile l) (compile r))
compile (LtW args) = compFoldOptimization LtW Lt args -- (Lt (compile l) (compile r))
compile (GtEW args) = compFoldOptimization GtEW GtE args -- (GtE (compile l) (compile r))
compile (LtEW args) = compFoldOptimization LtEW LtE args -- (LtE (compile l) (compile r))
compile (CondW tst thn els) = (Cond (compile tst) (compile thn) (compile els))
compile (WithW name namedExpr body) = (App (Fun name (compile body)) (compile namedExpr))

compile (AppW funExpr argExprs) =
    if (length argExprs == 0)
        then error "Compile - Nullary Application"
        else if (length argExprs == 1)
            then (App (compile funExpr) (compile (head argExprs)))
            else (App (compile (AppW funExpr (getAllButLast argExprs))) (compile (last argExprs)))

compile (FunW paramNames body) =
    if (length paramNames) == 0
        then error "Compile - Nullary Function"
        else if (length paramNames == 1)
            then (Fun (head paramNames) (compile body))
            else (Fun (head paramNames) (compile (FunW (tail paramNames) body)))

compile (FirstW lst) = (First (compile lst))
compile (RestW lst) = (Rest (compile lst))
-- compile (FunAppW funName args) = (FunApp (Sym funName) (map compile args))
compile (ListW vals) = (ListE (map compile vals))
compile (ConsW l r) = (Cons (compile l) (compile r))
compile (AppendW l r) = (Append (compile l) (compile r))
compile (EmptyW lst) = (EmptyE (compile lst))
-- compile (MapW fun lst) = (MapE (compile fun) (compile lst))
-- compile (FilterW fun lst) = (FilterE (compile fun) (compile lst))
-- compile (BeginW lst) = leftFoldOptimization BeginW Begin lst


compileMap :: [WExpr] -> [Expr]
compileMap wEs = (map compile wEs)

-- Lookup symbol to see if in DS, if not check global function definitions
lookupDS :: LispVal -> [GlobalFunDef] -> DefSub -> ExprValue
lookupDS (Symbol s1) funDefs (MtSub) = interp (lookupFundefs s1 funDefs) funDefs (MtSub)
lookupDS (Symbol s1) funDefs (ASub s2 val rest) = 
    if s1 == s2
        then val
        else (lookupDS (Symbol s1) funDefs rest)
lookupDS _ _ _ = error "lookupDS malformed"


lookupFundefs :: String -> [GlobalFunDef] -> Expr
lookupFundefs s [] = error ("interp - free identifier" ++ s)
lookupFundefs s ((FundefG funName closure):xs) = 
    if funName == s
        then closure
        else (lookupFundefs s xs)

lookupFundef :: String -> [FunDef] -> FunDef
lookupFundef funName [] = error ("interp: undefined function: " ++ funName)
lookupFundef funName ((Fundef name args body):xs) = 
    if funName == name
        then (Fundef name args body)
        else (lookupFundef funName xs)


-- TODO abstract numOp to any operator, use generically in place of all of these
-- TRY TO FIX THIS
numOp :: ExprValue -> ExprValue -> (Integer -> Integer -> Integer) -> ExprValue
numOP (NumV l) (NumV r) fn = (NumV ((fn) l r))
numOp _ _ = (error "Wrong value given to numerical operator")

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

appEval :: ExprValue -> ExprValue -> [GlobalFunDef] -> ExprValue
appEval (ClosureV paramName body ds) argVal funDefs = (interp body funDefs (ASub paramName argVal ds))
appEval _ _ _ = error "expected function"

matchStrToBool :: String -> Bool
matchStrToBool "#t" = True
matchStrToBool "#f" = False
matchStrTobool _ = error "Boolean malformed"





defSubHelper :: [String] -> [GlobalFunDef] -> [Expr] -> DefSub -> DefSub
defSubHelper [] funDefs [] ds = (MtSub)
defSubHelper [] funDefs (x:xs) ds = error "interp: wrong arity"
defSubHelper (x:xs) funDefs [] ds = error "interp: wrong arity"
defSubHelper (x:xs) funDefs (b:bs) ds = (ASub x (interp b funDefs ds) (defSubHelper xs funDefs bs ds))


-- funAppHelper :: [FunDef] -> FunDef -> [Expr] -> DefSub -> ExprValue
-- funAppHelper funDefs (Fundef funName args body) argExprs ds = 
--     (interp body funDefs (defSubHelper args funDefs argExprs ds))


checkNumber :: ExprValue -> Number
checkNumber (NumV n) = n
checkNumber _ = error "comparator not supported for non numbers"


listOp :: Expr -> [Expr]
listOp (ListE lst) = lst
listOp s = error ("List operation applied to non list: " ++ (show s))


listOpV :: ExprValue -> [ExprValue]
listOpV (ListV lst) = lst


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
-- shortCircuit :: Expr -> Expr -> ExprValue
-- shortCircuit lhs rhs = _

-- data FunDef = Fundef String [String] Expr deriving (Eq, Show)

interp :: Expr -> [GlobalFunDef] -> DefSub -> ExprValue
interp (Numb n) _ _ = NumV n
interp (Boolean b) _ _ = BoolV (matchStrToBool b)
interp (Sym s) funDefs ds = lookupDS (Symbol s) funDefs ds
interp (Add lhs rhs) funDefs ds = numOpAdd (interp lhs funDefs ds) (interp rhs funDefs ds)
interp (Sub lhs rhs) funDefs ds = numOpSub (interp lhs funDefs ds) (interp rhs funDefs ds)
interp (Mult lhs rhs) funDefs ds = numOpMult (interp lhs funDefs ds) (interp rhs funDefs ds)
interp (Div lhs rhs) funDefs ds = numOpDiv (interp lhs funDefs ds) (interp rhs funDefs ds)
interp (Equal lhs rhs) funDefs ds = BoolV ((interp lhs funDefs ds) == (interp rhs funDefs ds))
interp (Lt lhs rhs) funDefs ds = BoolV ((checkNumber (interp lhs funDefs ds)) < (checkNumber (interp rhs funDefs ds)))
interp (Gt lhs rhs) funDefs ds = BoolV ((checkNumber (interp lhs funDefs ds)) > (checkNumber (interp rhs funDefs ds)))
interp (LtE lhs rhs) funDefs ds = BoolV ((checkNumber (interp lhs funDefs ds)) <= (checkNumber (interp rhs funDefs ds)))
interp (GtE lhs rhs) funDefs ds = BoolV ((checkNumber (interp lhs funDefs ds)) <= (checkNumber (interp rhs funDefs ds)))
interp (App funExpr argExpr) funDefs ds = appEval (interp funExpr funDefs ds) (interp argExpr funDefs ds) funDefs
interp (Fun paramName body) _ ds = (ClosureV paramName body ds)
interp (Cond test thn els) funDefs ds = -- expand to any number of conditions
    if (evalTestBool (interp test funDefs ds))
        then (interp thn funDefs ds)
        else (interp els funDefs ds)
-- interp (FunApp (Sym funName) args) funDefs ds = funAppHelper funDefs (lookupFundef funName funDefs) args ds -- expand interp to take fundefs
interp (ListE vals) funDefs ds = (ListV (map (\x -> (interp x funDefs ds)) vals))
-- interp (First lst) funDefs ds = (interp (head (listOp lst)) funDefs ds) -- need to split on the exprvalue cases for the result
-- interp (Rest lst) funDefs ds = (interp (ListE (tail (listOp lst))) funDefs ds) -- same as first, do listOp function or something

interp (First lst) funDefs ds = (head (listOpV (interp lst funDefs ds)))-- need to split on the exprvalue cases for the result
interp (Rest lst) funDefs ds = (ListV (tail (listOpV (interp lst funDefs ds))))


-- interp (Rest lst) funDefs ds = (ListV ())
-- interp (Cons l r) funDefs ds = (interp (ListE (l : (listOp r))) funDefs ds) -- TODO expand to any number of arguments
interp (Cons l r) funDefs ds = (ListV ((interp l funDefs ds) : (listOpV (interp r funDefs ds)))) -- TODO expand to any number of arguments
-- interp (Append l r) funDefs ds = (interp (ListE ((listOp l) ++ (listOp r))) funDefs ds) -- TODO expand to any number of arguments
interp (Append l r) funDefs ds = (ListV ((listOpV (interp l funDefs ds)) ++ (listOpV (interp r funDefs ds)))) 
interp (Not cond) funDefs ds = (BoolV (not (boolOp (interp cond funDefs ds))))
interp (EmptyE lst) funDefs ds = (BoolV (length (listOpV (interp lst funDefs ds)) == 0))
-- interp (MapE fn lst) funDefs ds = (ListV (map (\x -> (interp (App fn x) funDefs ds)) (listOp lst)))
-- interp (FilterE fn lst) funDefs ds = 
    -- (ListV (map (\x -> (interp x funDefs ds)) (filter (\x -> (interp (App fn x) funDefs ds) == (BoolV True)) (listOp lst))))
    

    -- (ListV (filter (\x -> ((interp (App fn x) funDefs ds) == (BoolV True))) (listOp (interp lst funDefs ds))))
    -- (ListV (filter (\x -> ((interp (App fn x) funDefs ds) == (BoolV True))) (listOp lst))) 

-- interp (And conds) funDefs ds = 
--     if (interp (head conds) funDefs ds) == (BoolV False)
--         then (BoolV False)
--         else if (interp (And (tail conds)) funDefs ds) == (BoolV True)
--             then (BoolV True)
--             else (BoolV False)


interp (And lhs rhs) funDefs ds =  -- TODO check if this is actually a necessary optimization?
    if (interp lhs funDefs ds) == (BoolV False)
        then (BoolV False)
        else if (interp rhs funDefs ds) == (BoolV True)
            then (BoolV True)
            else (BoolV False)
interp (Or lhs rhs) funDefs ds = 
    if (interp lhs funDefs ds) == (BoolV True)
        then (BoolV True)
        else if (interp rhs funDefs ds) == (BoolV True)
            then (BoolV True)
            else (BoolV False)
    
-- interp _ _ _ = error "Interp - Not sure how we got here!"



-- interp (With ) -- TODO figure out this with thing


-- TODO pick up here
interpWrap :: Expr -> [GlobalFunDef] -> ExprValue
interpWrap s funDefs = interp s funDefs (MtSub)

multipleInterp :: [Expr] -> [GlobalFunDef] -> [ExprValue]
multipleInterp s funDefs = (map (\x -> (interpWrap x funDefs)) s)

interpVal :: ExprValue -> String
interpVal (NumV n) = show n
interpVal (BoolV b) = show b
interpVal (ListV vals) = show (map interpVal vals)
interpVal (ClosureV _ _ _) = "internal function"

multipleInterpVal :: [ExprValue] -> [String]
multipleInterpVal evs = (map interpVal evs)

{-
TODO

add parens
split up functions and exprs
then interp

-}
eval :: String -> [String]
eval expr = 
    (multipleInterpVal 
    (multipleInterp
    (compileMap (parserWrapper (getAllExprs (lexer expr)))) 
    (getFunDefs expr)))


-- main :: IO ()
-- main = do

--     let expr = "(map (lambda (a) (+ 5 a)) (list 1 2 3 4))"

--     print (parserWrapper (getAllExprs (lexer expr)))


--     let expr = "(+ 1 2 3)"
--     let expr2 = "(* 2 3 4)"
--     let expr3 = "(/ 100 10 5)"
--     let expr4 = "(- 100 90 5)"

--     let expr5 = "(and #t #t)"
--     let expr6 = "(and #t #t #t #t #t #f)"
--     let expr7 = "(and #f #t #t)"
--     let expr8 = ""

--     -- be able to bind variables for the course of a file and substitute them in
--     -- let expr8 = "(define app (lambda (arg1 arg2) (string-append arg1 arg2)))"
--     -- print (eval expr8)


--     print (eval expr5)
--     print (eval expr6)
--     print (eval expr7)

--     -- print (100 `div` 10 `div` 5)

--     -- print (compileMap (parserWrapper (getAllExprs (lexer expr4))))

--     -- print (lexer expr)

--     -- let expr = "(* 3 5)"
--     -- let expr2 = "(/ 3 5)"



--     print (eval expr)
--     print (eval expr2)
--     print (eval expr3)
--     print (eval expr4)
--     -- print (eval expr2)





--     (define (fib n)
--   (if (= n 1)
--       0
--       (if (= n 2)
--           1
--           (+ (fib (- n 1)) (fib (- n 2))))))



    -- print (eval expr6)


    -- print (lexer expr)
    -- print (getAllExprs (lexer expr))
    -- print (parserWrapper (getAllExprs (lexer expr)))
    -- print (compileMap (parserWrapper (getAllExprs (lexer expr))))
    -- print (eval expr)

    -- print (eval expr)

    -- print (getFunDefs expr)

    -- print (interpWrap (compile (parser (lexer expr))) [])



    -- let testExpr = "(define (my-func arg1 arg2) (+ arg1 arg2))"

    -- print (parser (lexer testExpr))


    -- let testExpr2 = "(my-func 25 35)"

    -- print (parser (lexer testExpr2))

    -- let testExpr3 = "(+ 5 5)"

    -- print (parser (lexer testExpr3))

    -- print (lexer testExpr2)
    -- print (lexer testExpr3)

    -- let testExpr3 = "(begin (+ 5 5) (+ 5 5))"

    -- let testExpr4 = "(define (func1 x y) (+ x y)) (define (func2 x y) (+ x y)) (+ 5 5)"

    -- let test = List [List [Symbol "define",List [Symbol "my-func",Symbol "arg1",Symbol "arg2"],List [Symbol "+",Symbol "arg1",Symbol "arg2"]],List [Symbol "define",List [Symbol "my-func2",Symbol "arg1",Symbol "arg2"],List [Symbol "+",Symbol "arg1",Symbol "arg2"]]]

    -- print (getAllFunDefs testExpr4)

    -- print (parseFunDefs (getAllFunDefs (lexer testExpr4)))

    -- print (getAllExprs (lexer testExpr4))

    -- print (lexer "(+ 10 (-5 6))")
    -- print (lexer testExpr4)

    -- List [Symbol "define",List [Symbol "func1",Symbol "x",Symbol "y"],List [Symbol "+",Symbol "x",Symbol "y"]]

    -- print (lexer testExpr)
    -- print (lexer testExpr2)
    -- print (lexer testExpr3)

    -- let test = AppW (FunW ["x"] (AddW (SymW "x") (NumbW 10))) [NumbW 10]

    -- print (length [NumbW 10])

    -- print (compile test)

    -- print (parser (lexer expr4))

    -- AppW (FunW ["x"] (AddW (SymW "x") (NumbW 10))) [NumbW 10]

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
