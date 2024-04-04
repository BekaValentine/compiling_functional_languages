module ExampleLanguage.Main where

import ExampleLanguage.Syntax
import ExampleLanguage.BigStep
import ExampleLanguage.SmallStep
import ExampleLanguage.Compiled
import qualified ExampleLanguage.Compiled as C
import qualified ExampleLanguage.CompiledMonadic as CM
import qualified ExampleLanguage.Flattened1 as F1
import qualified ExampleLanguage.Flattened2 as F2

main :: IO ()
main = do
    print (evalProgram0 ex)
    print (execProgram0 ex)
    print (compile extm)
    print (C.exec (Running [] [] [] (C.compile extm)))
    print (CM.exec0 (CM.compile extm))
    print (F1.exec0 (F1.compile extm))
    print (F2.exec0 (F2.compile extm))

{-
foo :=
        let b = True
        in if b
        then 1
        else 2

bar :=
        1 + $foo
-}
extm :: Term
extm = Plus (Let "b" BoolFalse
                (IfThenElse
                    (Var "b")
                    (Number 1)
                    (Number 2)))
            (Number 3)

ex :: Program
ex = [
        Definition "foo"
            extm,
        Definition "bar"
            (Plus (Number 1)
                (DeclaredName "foo"))
        ]