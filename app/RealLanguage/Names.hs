module RealLanguage.Names where

newtype TypeName = TypeName String
    deriving (Show, Eq)

newtype ConName = ConName String
    deriving (Show)

newtype TermName = TermName String
    deriving (Show)

newtype VarName = VarName String
    deriving (Show, Eq)

newtype FunName = FunName String
    deriving (Show, Eq)