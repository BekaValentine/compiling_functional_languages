module ExampleLanguage.SmallStep where

import ExampleLanguage.BigStep (Value(..), GlobalEnv, Env)
import ExampleLanguage.Syntax


data Frame
    = InPlusL Term
    | InPlusR
    | InIfTest Term Term
    | InLetExpr Name Term
    | InLetBody Name
    deriving (Show)

type Stack = [Frame]
type ReturnStack = [Value]

data MachineState
    = Enter Stack GlobalEnv Env ReturnStack Term
    | Exit Stack GlobalEnv Env ReturnStack
    | Halt ReturnStack

execTerm :: GlobalEnv -> Env -> Term -> Value
execTerm genv env x = exec (Enter [] genv env [] x)

exec :: MachineState -> Value
exec (Enter s genv env ret x) =
    case x of
        Number i -> exec (Exit s genv env (NumberVal i:ret))
        Plus y z -> exec (Enter (InPlusL z:s) genv env ret y)
        BoolTrue -> exec (Exit s genv env (BooleanVal True:ret))
        BoolFalse -> exec (Exit s genv env (BooleanVal False:ret))
        IfThenElse t c a ->
            exec (Enter (InIfTest c a:s) genv env ret t)
        Let n x' y ->
            exec (Enter (InLetExpr n y:s) genv env ret x')
        Var n ->
            case lookup n env of
                Nothing -> error "Unbound var"
                Just v -> exec (Exit s genv env (v:ret))
        DeclaredName n ->
            case lookup n genv of
                Nothing -> error "Unknown global var"
                Just v -> exec (Exit s genv env (v:ret))
exec (Exit s genv env ret) =
    case s of
        [] -> exec (Halt ret)
        f:s' -> case f of
            InPlusL y ->
                exec (Enter (InPlusR:s') genv env ret y)
            InPlusR ->
                case ret of
                    (v:u:ret') ->
                        case (u, v) of
                            (NumberVal u', NumberVal v') ->
                                exec (Exit s' genv env (NumberVal (u' + v'):ret'))
                            _ -> error "Plus of non-nums"
                    _ -> error "Not enough return values!"
            InIfTest c a ->
                case ret of
                    (v:ret') ->
                        case v of
                            BooleanVal True -> exec (Enter s' genv env ret' c)
                            BooleanVal False -> exec (Enter s' genv env ret' a)
                            _ -> error "If non-boolean"
                    _ -> error "Not enough return values!"
            InLetExpr n y ->
                case ret of
                    (v:ret') ->
                        exec (Enter (InLetBody n:s') genv ((n,v):env) ret' y)
                    _ -> error "Not enough return values!"
            InLetBody n ->
                case env of
                    ((n',_):env') | n == n' ->
                        exec (Exit s' genv env' ret)
                    _ -> exec (Exit s' genv env ret)
exec (Halt ret) =
    case ret of
        [v] -> v
        _ -> error "Too many return values!"

execProgram0 = execProgram []

execProgram :: GlobalEnv -> Program -> GlobalEnv
execProgram genv [] = genv
execProgram genv (d:ds) =
    let genv' = execDeclaration genv d
    in execProgram genv' ds

execDeclaration :: GlobalEnv -> Declaration -> GlobalEnv
execDeclaration genv (Definition n x) =
    let v = execTerm genv [] x
    in (n,v):genv