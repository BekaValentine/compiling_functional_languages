module RealLanguage.Core where

import RealLanguage.Names
import RealLanguage.Types

data Term
    = Var VarName
    | DefVar TermName
    | Con ConName [Term]
    | Case [Term] [Clause]
    | Fun FunName
    | Apply Term Term
    | Ann Term Type
    deriving (Show)

data Clause
    = Clause [Pattern] Term
    deriving (Show)

data Pattern
    = VarPat VarName
    | ConPat ConName [Pattern]
    deriving (Show)