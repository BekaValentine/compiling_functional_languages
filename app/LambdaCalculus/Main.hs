module LambdaCalculus.Main where

import LambdaCalculus.Basic.Syntax
import LambdaCalculus.Basic.BigStep
import qualified LambdaCalculus.DeBruijn.Syntax as DB
import qualified LambdaCalculus.DeBruijn.BigStep as DB

main :: IO ()
main = do
    print extm
    print (eval0 extm)
    print (evalProgram0 ex)
    print dbtm
    print (DB.eval0 dbtm)

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

{-
(\. if `0 1 2) false
-}
dbtm :: DB.Term
dbtm = DB.Apply
            (DB.Lambda
                (DB.IfThenElse
                    (DB.Var 0)
                    (DB.Number 1)
                    (DB.Number 2)))
            DB.BoolFalse