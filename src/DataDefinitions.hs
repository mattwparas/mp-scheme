{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DataDefinitions where

import qualified Data.Map as Map

import Control.Monad.IO.Class
import Control.Monad.Reader

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
    | DoubW Double
    | CharW Char
    | BooleanW String
    | StringW String
    | SymW [Char]
    | AddW [WExpr]
    | SubW [WExpr]
    | MultW [WExpr]
    | DivW [WExpr]
    | EqualW [WExpr]
    | LtW [WExpr]
    | GtW [WExpr]
    | LtEW [WExpr]
    | GtEW [WExpr]
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
    | UserInputW
    | PrintLnW WExpr
    | GetW WExpr
    | BeginW [WExpr]
    | LetW [(String, WExpr)] WExpr
    | StructW [(String, WExpr)]
    | StructGetW String WExpr
    | CastExpressionW WExpr ExprValueT
    | CheckTypeW WExpr ExprValueT
    deriving (Eq, Show)

      
data Expr = 
    Numb Integer
    | Doub Double
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
    | UserInput
    | PrintLn Expr
    | GetE Expr
    | BeginE [Expr]
    | StructE [(String, Expr)]
    | StructGetE String Expr
    | CastExpression Expr ExprValueT
    | CheckTypeE Expr ExprValueT
    deriving (Eq, Show)


newtype Eval a = Eval { unEval :: ReaderT WExpr IO a }
    deriving (Monad, Functor, Applicative, MonadIO)


-- Replace every instance of [FunDef] with HashMap
data GlobalFunDef = FundefG String Expr deriving (Eq, Show)


type FunCtx = Map.Map String Expr
type DefSub = Map.Map String ExprValue


data ExprValueT =
    NumberT
    | IntT
    | DoubT
    | CharT
    | BoolT
    | ClosureT
    | ListT
    | StringT
    | StructT
    | NullT
    deriving (Eq, Show)

data ExprValue =
    NumV Integer
    | DoubV Double
    | CharV Char
    | BoolV Bool
    | ClosureV String Expr DefSub
    | ListV [ExprValue]
    | StringV String
    | StructV [(String, ExprValue)] -- TODO check this out maybe?
    | NullV
    deriving (Eq, Show)