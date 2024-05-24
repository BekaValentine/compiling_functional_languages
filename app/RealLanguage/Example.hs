module RealLanguage.Example where

import RealLanguage.Surface
import RealLanguage.Names
import RealLanguage.Types

{-
data Nat;
con Zero : () ~> Nat;
con Suc : (Nat) ~> Nat;

term $plus : Nat -> (Nat -> Nat);
term $plus = \x -> \y ->
  case x of
    | Zero() -> y
    | Suc(x') -> Suc($plus x' y)
    ;
-}

program :: Program
program = Program
    [ DataDecl (TypeName "Nat")
    , ConDecl (ConName "Zero")
        (ConSig [] (ConTy (TypeName "Nat")))
    , ConDecl (ConName "Suc")
        (ConSig [ConTy (TypeName "Nat")] (ConTy (TypeName "Nat")))
    , TermDecl (TermName "plus")
        (FunTy (ConTy (TypeName "Nat"))
            (FunTy (ConTy (TypeName "Nat"))
                (ConTy (TypeName "Nat"))))
    , TermDef (TermName "plus")
        (Lambda (VarName "x")
            (Lambda (VarName "y")
                (Case [Var (VarName "x")]
                    [ Clause [ConPat (ConName "Zero") []]
                        (Var (VarName "y"))
                    , Clause [ConPat (ConName "Suc") [VarPat (VarName "x'")]]
                        (Con (ConName "Suc")
                            [Apply
                                (Apply (DefVar (TermName "plus"))
                                    (Var (VarName "x'")))
                                (Var (VarName "y"))])
                    ])))
    ]