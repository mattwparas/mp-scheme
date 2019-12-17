{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DataDefinitions where

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


data LispVal
    = Symbol String
      | StringVal String
      | List [LispVal]
      | ListVal [LispVal] -- direct translation to lists
      | VectorVal [LispVal]
      deriving (Eq, Show)


type Symbol = String
type Number = Integer

data WExpr =
    NumbW Integer
    | CharW Char
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
    | StringToListW WExpr
    | ListToStringW WExpr
    deriving (Eq, Show)
      
data Expr = 
    Numb Integer
    | CharE Char
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
    -- | Fun String Expr -- Fun [String] Expr - TODO
    -- | App Expr Expr -- App Expr [Expr] - TODO
    | Fun [String] Expr
    | App Expr [Expr]
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
    | StringToList Expr
    | ListToString Expr
    deriving (Eq, Show)


newtype Eval a = Eval { unEval :: ReaderT WExpr IO a }
    deriving (Monad, Functor, Applicative, MonadIO)
      
      
-- Replace this with a HashMap
data DefSub = MtSub | ASub Symbol ExprValue DefSub deriving (Eq, Show)

-- Replace every instance of [FunDef] with HashMap
data GlobalFunDef = FundefG String Expr deriving (Eq, Show)

data ExprValue =
    NumV Integer
    | CharV Char
    | BoolV Bool
    | ClosureV String Expr DefSub
    | ListV [ExprValue]
    | StringV String
    deriving (Eq, Show)