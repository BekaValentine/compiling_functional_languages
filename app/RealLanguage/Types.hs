module RealLanguage.Types where

import RealLanguage.Names

data ConSig = ConSig [Type] Type
    deriving (Show)

data Type
    = ConTy TypeName
    | FunTy Type Type
    deriving (Show, Eq)