module RealLanguage.Surface where

import RealLanguage.Names
import RealLanguage.Types

newtype Program
    = Program [Statement]
    deriving (Show)

data Statement
    = DataDecl TypeName
    | ConDecl ConName ConSig
    | TermDecl TermName Type
    | TermDef TermName Term
    deriving (Show)

data Term
    = Var VarName
    | DefVar TermName
    | Con ConName [Term]
    | Case [Term] [Clause]
    | Lambda VarName Term
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


patternVars :: Pattern -> [VarName]
patternVars (VarPat vn) = [vn]
patternVars (ConPat _ ps) = ps >>= patternVars