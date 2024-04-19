module Main where

import RealLanguage.Syntax
import RealLanguage.Elaboration

main :: IO ()
main = do
    print $ runElab (prove (TypeValid d (ConTy (TypeName "Float"))))
    where
        d = [
            ("Int", TypeNameExists)
            ]