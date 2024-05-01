module Main where

import RealLanguage.Syntax
import RealLanguage.Elaboration

main :: IO ()
main = do
    print $ runElab (goal gl)
    where
        d = [
            ("Int", TypeNameExists)
            ]
        
        g = []

        a = FunTy (ConTy (TypeName "Int")) (ConTy (TypeName "Int"))

        m = Lambda (VarName "x") (Var (VarName "x"))

        gl = CheckTerm d g a m