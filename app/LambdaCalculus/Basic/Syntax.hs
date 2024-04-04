module LambdaCalculus.Basic.Syntax where

type Name = String
type Program = [Declaration]

data Declaration
    = Definition Name Term

data Term
    = Number Int
    | Plus Term Term
    | BoolTrue
    | BoolFalse
    | IfThenElse Term Term Term
    | Let Name Term Term
    | Lambda Name Term
    | Apply Term Term
    | Var Name
    | DeclaredName Name
    deriving (Show)