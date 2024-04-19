module RealLanguage.Syntax where

newtype Program
    = Program [Statement]
    deriving (Show)

data Statement
    = DataDecl TypeName
    | ConDecl ConName ConSig
    | TermDecl TermName Type
    | TermDef TermName Term
    deriving (Show)
    
newtype TypeName = TypeName String
    deriving (Show)

newtype ConName = ConName String
    deriving (Show)

newtype TermName = TermName String
    deriving (Show)

data ConSig = ConSig [Type] Type
    deriving (Show)

data Type
    = ConTy TypeName
    | FunTy Type Type
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

newtype VarName = VarName String
    deriving (Show)

data Clause
    = Clause [Pattern] Term
    deriving (Show)

data Pattern
    = VarPat VarName
    | ConPat ConName [Pattern]
    deriving (Show)