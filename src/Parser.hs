module Parser where

import DataDefinitions
import Helpers
import Lexer

{------------- Parsing -------------}

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

letExtract :: LispVal -> (String, WExpr)
letExtract (List ((Symbol s):body:[])) = (s, (parser body))
letExtract _ = error "malformed let-extract"

letHelper :: [LispVal] -> WExpr
letHelper l = (LetW (map letExtract (unWrapBracket (head l))) (parser (last l)))

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


structHelper :: [LispVal] -> WExpr
structHelper lst = 
    (StructW 
    (map (\x -> ((extractSymbol (head (unWrapBracket x))), (parser (last (unWrapBracket x))))) lst))

switchSymbol :: String -> [LispVal] -> WExpr
switchSymbol "+" lv = (AddW (map parser lv)) -- TODO error checking

-- switchSymbol "applytest" lv = (AddW (map parser lv)) -- TODO error checking


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

switchSymbol "if" lv = (CondW (parser (lv !! 0)) (parser (lv !! 1)) (parser (lv !! 2))) -- TODO come back
switchSymbol "lambda" lv = (FunW (map extractSymbol (funHelper (head lv))) (parser (last lv))) -- change to accept multiple arguments
switchSymbol "λ" lv = (FunW (map extractSymbol (funHelper (head lv))) (parser (last lv)))
switchSymbol "with" lv = letHelper lv
switchSymbol "let" lv = letHelper lv
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

-- Checking Types
switchSymbol "integer?" lv = (CheckTypeW (parser (head lv)) (IntT)) -- should be ExprValues, not WExpr
switchSymbol "double?" lv = (CheckTypeW (parser (head lv)) (DoubT))
switchSymbol "string?" lv = (CheckTypeW (parser (head lv)) (StringT))
switchSymbol "char?" lv = (CheckTypeW (parser (head lv)) (CharT))
switchSymbol "list?" lv = (CheckTypeW (parser (head lv)) (ListT))
switchSymbol "closure?" lv = (CheckTypeW (parser (head lv)) (ClosureT))
switchSymbol "bool?" lv = (CheckTypeW (parser (head lv)) (BoolT))
switchSymbol "struct?" lv = (CheckTypeW (parser (head lv)) (StructT))
switchSymbol "number?" lv = (CheckTypeW (parser (head lv)) (NumberT))


switchSymbol "user-input" lv = (UserInputW)
switchSymbol "println" lv = (PrintLnW (parser (head (lv))))
switchSymbol "get!" lv = (GetW (parser (head lv)))
switchSymbol "begin" lv = (BeginW (map parser lv))
switchSymbol "struct" lv = structHelper lv

-- TODO come back to this with state!
switchSymbol "struct-get" lv = (StructGetW (extractSymbol (head lv)) (parser (last lv)))

-- TOOD come back to this, need to just rearrange when I'm calling this - it can't be here
-- needs tobe in interpreter
switchSymbol "string->jsexpr" lv = (StringToJsexprW (parser (head lv)))


-- switchSymbol "struct-set" lv = (StructSetW (extractSymbol (head lv)) (parser (lv !! 1)) (parser (lv !! 2)))


-- Casting Types
switchSymbol "string->double" lv = (CastExpressionW (parser (head lv)) (DoubT))
switchSymbol "string->integer" lv = (CastExpressionW (parser (head lv)) (IntT))
switchSymbol "string->list" lv = (CastExpressionW (parser (head lv)) (ListT))
switchSymbol "list->string" lv = (CastExpressionW (parser (head lv)) (StringT))
switchSymbol "string->number" lv = (CastExpressionW (parser (head lv)) (NumberT))
switchSymbol "integer->double" lv = (CastExpressionW (parser (head lv)) (DoubT))
switchSymbol "double->integer" lv = (CastExpressionW (parser (head lv)) (IntT))

switchSymbol "apply" lv = (ApplyW (extractSymbol (head lv)) (parser (last lv)))

switchSymbol s lv = (AppW (SymW s) (map parser lv)) -- TODO instead of this, go through the list of deferred subst FIRST then go through the fundefs

applyHelper :: [LispVal] -> WExpr
applyHelper ((Symbol fn):xs) = switchSymbol fn xs


getString :: WExpr -> String
getString (StringW s) = s
getString e = error ("wrong value given to get string" ++ (show e))

appHelper :: [LispVal] -> WExpr
appHelper lv = (AppW (parser (head lv)) (map parser (tail lv)))

isBoolean :: String -> Bool
isBoolean s = 
    (s == "#t") || (s == "#f") || 
    (s == "#true") || (s == "#false") || 
    (s == "#True") || (s == "#False") ||
    (s == "true") || (s == "false")

unWrapBracket :: LispVal -> [LispVal]
unWrapBracket (ListVal l) = l
unWrapBracket e = error ("unwrapping a bracketed value threw an error: " ++ (show e))

formatChar :: String -> Char
formatChar "#/" = ' '
formatChar s = (last s)

group :: Int -> [LispVal] -> [[LispVal]]
group _ [] = []
group n l
  | n > 0 = (take n l) : (group n (drop n l))
  | otherwise = error "Negative or zero n"


extractString :: LispVal -> String
extractString (StringVal s) = s
extractString e = error ("using extract string on not a string: " ++ (show e))

braceHelper :: [LispVal] -> WExpr
braceHelper lst = StructW (map (\x -> ((extractString (head x)), (parser (last x)))) (group 2 lst))

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
parser (ListJson lst) = braceHelper lst
parser _ = error "pattern not matched"

parserWrapper :: [LispVal] -> [WExpr]
parserWrapper lv = (map parser lv)
