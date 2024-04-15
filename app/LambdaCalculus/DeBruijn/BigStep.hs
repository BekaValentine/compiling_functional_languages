module LambdaCalculus.DeBruijn.BigStep where

import LambdaCalculus.DeBruijn.Syntax

data Value
    = NumberVal Int
    | BooleanVal Bool
    | Closure Env Term
    deriving (Show)

type GlobalEnv = [(Name,Value)]
type Env = [Value]

eval0 :: Term -> Value
eval0 = eval [] []

eval :: GlobalEnv -> Env -> Term -> Value
eval genv env (Number i) = NumberVal i
eval genv env (Plus x y) =
    case (eval genv env x, eval genv env y) of
        (NumberVal u, NumberVal v) ->
            NumberVal (u + v)
        _ -> error "Plus of non-nums"
eval genv env BoolTrue = BooleanVal True
eval genv env BoolFalse = BooleanVal False
eval genv env (IfThenElse t c a) =
    case eval genv env t of
        BooleanVal b ->
            if b
                then eval genv env c
                else eval genv env a
        _ -> error "If on non-bool"
eval genv env (Let x y) =
    let newEnv = eval genv env x:env
    in eval genv newEnv y
eval genv env (Lambda x) = Closure env x
eval genv env (Apply x y) =
    case eval genv env x of
        Closure env' x' ->
            let v = eval genv env y
            in eval genv (v:env') x'
        _ -> error "Cannot apply non-function"
eval genv env (Var i) =
    env !! i
eval genv env (DeclaredName n) =
    case lookup n genv of
        Nothing -> error "Unknown declared name"
        Just v -> v


evalProgram0 = evalProgram []

evalProgram :: GlobalEnv -> Program -> GlobalEnv
evalProgram genv [] = genv
evalProgram genv (d:ds) =
    let genv' = evalDeclaration genv d
    in evalProgram genv' ds

evalDeclaration :: GlobalEnv -> Declaration -> GlobalEnv
evalDeclaration genv (Definition n x) =
    (n, eval genv [] x) : genv