module ExampleLanguage.Compiled where

import ExampleLanguage.BigStep (Value(..), GlobalEnv, Env)
import ExampleLanguage.Syntax

data Instr
    = NUMBER Int
    | PLUS
    | BOOLTRUE
    | BOOLFALSE
    | IF Code Code
    | LET Name
    | FREE Name
    | VAR Name
    | DECLAREDNAME Name
    deriving (Show)

type Code = [Instr]

{-

Plus (Plus (Lit 1) (Lit 2)) (Lit 3)

    +
   / \
  +   3
 / \
1   2

1 [2 [+ []], 3 [+ []]]

Lit 1 (Lit 2 (Plus (Lit 3 (Plus Top))))

let x = 1 in
let y = 2 in
let z = x + y in
let w = 3 in
let q = z + w in
    q

G ni p : A*B   G, x : A, y : B => M : C
---------------------------------
G => let (x,y) = p in M : C

G ni m : Nat   G ni n : Nat   G, z : Nat => M : C
----------------
G => let z = m + n in M : C

-}

type ReturnStack = [Value]

data MachineState
    = Running GlobalEnv Env ReturnStack Code
    | Halt ReturnStack

compile :: Term -> Code
compile (Number i) = [NUMBER i]
compile (Plus x y) = compile x ++ compile y ++ [PLUS]
compile BoolTrue = [BOOLTRUE]
compile BoolFalse = [BOOLFALSE]
compile (IfThenElse t c a) = compile t ++ [IF (compile c) (compile a)]
compile (Let n x y) = compile x ++ [LET n] ++ compile y ++ [FREE n]
compile (Var n) = [VAR n]
compile (DeclaredName n) = [DECLAREDNAME n]

execTerm :: GlobalEnv -> Env -> Term -> Value
execTerm genv env x = exec (Running genv env [] (compile x))

exec :: MachineState -> Value
exec (Running genv env ret []) = exec (Halt ret)
exec (Running genv env ret (instr:code)) =
    case instr of
        NUMBER i -> exec (Running genv env (NumberVal i:ret) code)
        PLUS {- y z -} -> case ret of
            (v:u:ret') ->
                case (u, v) of
                    (NumberVal u', NumberVal v') ->
                        exec (Running genv env (NumberVal (u'+v'):ret') code)
                    _ -> error "Plus non-num"
            _ -> error "Not enough values!"
        BOOLTRUE -> exec (Running genv env (BooleanVal True:ret) code)
        BOOLFALSE -> exec (Running genv env (BooleanVal False:ret) code)
        IF {-t-} c a ->
            case ret of
                (v:ret') ->
                    case v of
                        BooleanVal b ->
                            exec (Running genv env ret'
                                    ((if b then c else a)++code))
                        _ -> error "If non-boolean"
                _ -> error "Not enough values!" 
        LET n {-x' y-} ->
            case ret of
                (v:ret') ->
                    exec (Running genv ((n,v):env) ret' code)
                _ -> error "Not enough values!"
        FREE n ->
            case env of
                ((n',_):env') | n == n' -> exec (Running genv env' ret code)
                _ -> exec (Running genv env ret code)
        VAR n ->
            case lookup n env of
                Nothing -> error "Unbound var"
                Just v -> exec (Running genv env (v:ret) code)
        DECLAREDNAME n ->
            case lookup n genv of
                Nothing -> error "Unknown global var"
                Just v -> exec (Running genv env (v:ret) code)
-- exec (Exit s genv env ret) =
--     case s of
--         [] -> exec (Halt ret)
--         f:s' -> case f of
--             InPlusL y ->
--                 exec (Enter (InPlusR:s') genv env ret y)
--             InPlusR ->
--                 case ret of
--                     (v:u:ret') ->
--                         case (u, v) of
--                             (NumberVal u', NumberVal v') ->
--                                 exec (Exit s' genv env (NumberVal (u' + v'):ret'))
--                             _ -> error "Plus of non-nums"
--                     _ -> error "Not enough return values!"
--             InIfTest c a ->
--                 case ret of
--                     (v:ret') ->
--                         case v of
--                             BooleanVal True -> exec (Enter s' genv env ret' c)
--                             BooleanVal False -> exec (Enter s' genv env ret' a)
--                             _ -> error "If non-boolean"
--                     _ -> error "Not enough return values!"
--             InLetExpr n y ->
--                 case ret of
--                     (v:ret') ->
--                         exec (Enter (InLetBody n:s') genv ((n,v):env) ret' y)
--                     _ -> error "Not enough return values!"
--             InLetBody n ->
--                 case env of
--                     ((n',_):env') | n == n' ->
--                         exec (Exit s' genv env' ret)
--                     _ -> exec (Exit s' genv env ret)
exec (Halt ret) =
    case ret of
        [v] -> v
        _ -> error "Too many return values!"

{-
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
    -}