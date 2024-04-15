module LambdaCalculus.DeBruijn.Syntax where

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
    | Let Term Term
    | Lambda Term
    | Apply Term Term
    | Var Int
    | DeclaredName Name
    deriving (Show)