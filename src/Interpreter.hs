module Interpreter where

import DataDefinitions
import Primitives
import Helpers
import Lexer
import Compiler
import Parser

import Data.Map (Map)
import qualified Data.Map as Map


import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Exception hiding (handle, try)
import System.Directory
import System.IO as SIO
import Control.Monad.Reader
import Network.HTTP




-- Lookup symbol to see if in DS, if not check global function definitions
lookupDS :: LispVal -> FunCtx -> DefSub -> Eval ExprValue
lookupDS (Symbol s1) funDefs ds = 
    if Map.member s1 ds
        then return (ds Map.! s1)
        else (interp (lookupFundefs s1 funDefs) funDefs Map.empty)
lookupDS _ _ _ = error "lookupDS malformed"


lookupFundefs :: String -> FunCtx -> Expr
lookupFundefs s ctx = 
    if Map.member s ctx
        then ctx Map.! s
        else error ("interp - free identifier " ++ s)


appEval :: ExprValue -> ExprValue -> FunCtx -> Eval ExprValue
appEval (ClosureV paramName body ds) argVal funDefs = 
    (interp body funDefs (Map.insert paramName argVal ds))
appEval _ _ _ = error "expected function"

appEvalNoArgs :: ExprValue -> FunCtx -> Eval ExprValue
appEvalNoArgs (ClosureV paramName body ds) funDefs = (interp body funDefs ds)
appEvalNoArgs _ _ = error "expected function"

beginHelper :: [Expr] -> FunCtx -> DefSub -> Eval ExprValue
beginHelper [] funDefs ds = error ("Empty begin statement!")
beginHelper (x:xs) funDefs ds = do
    res <- (interp x funDefs ds)
    if xs == []
        then do
            return res
        else beginHelper xs funDefs ds


-- TODO come back here
interp :: Expr -> FunCtx -> DefSub -> Eval ExprValue
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
    -- return (numOpComparator (<) l r)

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
    return (appendOp ll rr)

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

interp (UserInput) funDefs ds = do
    userInput

interp (PrintLn expr) funDefs ds = do
    res <- (interp expr funDefs ds)
    printLn res

interp (GetE expr) funDefs ds = do
    res <- (interp expr funDefs ds)
    wSlurp res

interp (BeginE exprs) funDefs ds = do
    beginHelper exprs funDefs ds

interp (StructE pairs) funDefs ds = do
    evals <- (mapM (\x -> (interp (snd x) funDefs ds)) pairs)
    let res = (map (\x -> (fst x)) pairs)
    return (StructV (zip res evals))

interp (StructGetE name struct) funDefs ds = do
    res <- interp struct funDefs ds
    return (getStructHelper name res)

interp (CastExpression expr t) funDefs ds = do
    exp <- interp expr funDefs ds
    return (castType exp t)

interp (CheckTypeE expr t) funDefs ds = do
    exp <- interp expr funDefs ds
    return (checkType exp t)


parseFunDef :: LispVal -> (String, Expr)
parseFunDef (List ((Symbol "define") : (List ((Symbol funName) : args) : body : []))) =
    (funName, (compile (FunW (map extractSymbol args) (parser body))))
parseFunDef _ = error "parseFunDef - malformed function"

parseFunDefs :: [LispVal] -> FunCtx
parseFunDefs x = Map.fromList (map parseFunDef x)

getFunDefs :: String -> FunCtx
getFunDefs s = (parseFunDefs (getAllFunDefs (lexer s)))

            -- TODO pick up here
interpWrap :: Expr -> FunCtx -> Eval ExprValue
interpWrap s funDefs = interp s funDefs (Map.empty)

multipleInterp :: [Expr] -> FunCtx -> Eval [ExprValue]
multipleInterp s funDefs = (mapM (\x -> (interpWrap x funDefs)) s)

multipleInterpVal :: [ExprValue] -> [String]
multipleInterpVal evs = (map interpVal evs)

multipleInterpValL :: Eval [ExprValue] -> Eval [String]
multipleInterpValL = liftM multipleInterpVal

eval :: String -> IO [String]
eval expr = do
    let res = (multipleInterp (compileMap (parserWrapper (getAllExprs (lexer expr)))) (getFunDefs expr))
    (runReaderT (unEval (multipleInterpValL res)) (StringW ""))

evalWithStdLib :: String -> String -> IO [String]
evalWithStdLib expr file = do
    let res = 
            (multipleInterp 
            (compileMap (parserWrapper (getAllExprs (lexer expr))))
            (Map.unions [(getFunDefs file), (getFunDefs expr)]))
    (runReaderT (unEval (multipleInterpValL res)) (StringW ""))
