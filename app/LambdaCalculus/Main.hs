module LambdaCalculus.Main where

import LambdaCalculus.Basic.Syntax
import LambdaCalculus.Basic.BigStep

main :: IO ()
main = do
    print extm
    print (eval0 extm)
    print (evalProgram0 ex)

extm :: Term
extm = Apply
            (Lambda "b"
                (IfThenElse
                    (Var "b")
                    (Number 1)
                    (Number 2)))
            BoolFalse

ex :: Program
ex = [
        Definition "foo"
            extm,
        Definition "bar"
            (Plus (Number 1)
                (DeclaredName "foo"))
        ]