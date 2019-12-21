module Compiler where

import DataDefinitions
import Helpers
import Parser


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
compile (DoubW n) = Doub n
compile (CharW c) = CharE c
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
compile (WithW name namedExpr body) = (App (Fun [name] (compile body)) [(compile namedExpr)])

compile (LetW symPairs body) = 
    if (length symPairs == 1)
        then (compile (WithW (fst (head symPairs)) (snd (head symPairs)) body))
        else (compile (WithW (fst (head symPairs)) (snd (head symPairs)) (LetW (tail symPairs) body)))

compile (SlurpW path) = (Slurp (compile path))
compile (SpitW path val) = (Spit (compile path) (compile val))

compile (AppW funExpr argExprs) =
    if (length argExprs == 0)
        then (App (compile funExpr) [])
        -- then error ("Compile - Nullary Application: " ++ (show funExpr)) -- TODO make it so you can use no arguments!
        -- then (App )
        else if (length argExprs == 1)
            then (App (compile funExpr) [(compile (head argExprs))])
            else (App (compile (AppW funExpr (getAllButLast argExprs))) [(compile (last argExprs))])

compile (FunW paramNames body) =
    if (length paramNames) == 0
        then (Fun [] (compile body))
        -- then error "Compile - Nullary Function" -- TODO make it so you can use no arguments!
        else if (length paramNames == 1)
            then (Fun [(head paramNames)] (compile body))
            else (Fun [(head paramNames)] (compile (FunW (tail paramNames) body)))

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

compile (StructW pairs) = (StructE (map (\x -> ((fst x), (compile (snd x)))) pairs))
compile (StructGetW name struct) = (StructGetE name (compile struct))
compile (UserInputW) = (UserInput)
compile (PrintLnW expr) = (PrintLn (compile expr))
compile (GetW expr) = (GetE (compile expr))
compile (BeginW exprs) = (BeginE (map compile exprs))

compile (CastExpressionW expr t) = (CastExpression (compile expr) t)
compile (CheckTypeW expr t) = (CheckTypeE (compile expr) t)
compile (StringToJsexprW expr) = (StringToJsexpr (compile expr))

compileMap :: [WExpr] -> [Expr]
compileMap wEs = (map compile wEs)