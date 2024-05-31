module RealLanguage.Core where

import Data.List ((\\))

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

freeVars :: Term -> [VarName]
freeVars (Var vn) = [vn]
freeVars (DefVar _) = []
freeVars (Con _ ms) = ms >>= freeVars
freeVars (Case ms cls) = (ms >>= freeVars) ++ (cls >>= freeVarsClause)
freeVars (Fun _) = []
freeVars (Apply m n) = freeVars m ++ freeVars n
freeVars (Ann m _) = freeVars m

freeVarsClause :: Clause -> [VarName]
freeVarsClause (Clause ps m) = freeVars m \\ (ps >>= patternVars)

patternVars :: Pattern -> [VarName]
patternVars (VarPat vn) = [vn]
patternVars (ConPat _ ps) = ps >>= patternVars