module Lexer where

import DataDefinitions
import Helpers


import Text.Parsec
import Text.Parsec.String
import Data.Either
import qualified Data.Functor.Identity as F
import qualified Text.Parsec.Prim as Prim
import Text.Parsec
       ((<|>), (<?>), many, many1, char, try, parse, sepBy, choice,
        between)
import Text.Parsec.Token
       (integer, float, whiteSpace, stringLiteral, makeTokenParser, charLiteral)
import Text.Parsec.Char (noneOf)
import Text.Parsec.Language (haskell)


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


-- unWrap :: LispVal -> [LispVal]
-- unWrap (List x) = x
-- unWrap _ = error "unwrap used on a non list"

lexer :: String -> LispVal
lexer s = (fromRight (Symbol "") (parse tProg "" (addParens s)))

addParens :: String -> String
addParens s = "(" ++ s ++ ")"


