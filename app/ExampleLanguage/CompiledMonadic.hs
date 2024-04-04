module ExampleLanguage.CompiledMonadic where

import Control.Monad.State
import Control.Monad.Except

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

type ExecState = (GlobalEnv, Env, ReturnStack)
type Execution a = StateT ExecState (Either String) a

execTerm :: Term -> Execution Value
execTerm x = exec (compile x)

pop :: Execution Value
pop = do
    (_, _, ret) <- get
    case ret of
        v:_ -> return v
        _ -> throwError "Not enough values!"

push :: Value -> Execution ()
push v = do
    (genv, env, ret) <- get
    put (genv, env, v:ret)

lookupVar :: Name -> Execution ()
lookupVar n = do
    (_, env, _) <- get
    case lookup n env of
        Nothing -> throwError "Unbound variable"
        Just v -> push v

lookupDeclaredName :: Name -> Execution ()
lookupDeclaredName n = do
    (genv, _, _) <- get
    case lookup n genv of
        Nothing -> throwError "Unknown defined name"
        Just v -> push v

bind :: Name -> Value -> Execution ()
bind n v = do
    (genv, env, ret) <- get
    put (genv, (n,v):env, ret)

free :: Name -> Execution ()
free n = do
    (genv, env, ret) <- get
    case env of
        (n',_):env' | n == n' -> put (genv, env', ret)
        _ -> put (genv, env, ret)

exec0 :: Code -> Either String Value
exec0 code = fmap fst (runStateT (exec code) ([], [], []))

exec :: Code -> Execution Value
exec [] = pop
exec (NUMBER i:code) = do
    push (NumberVal i)
    exec code
exec (PLUS:code) = do
    v <- pop
    u <- pop
    case (u, v) of
        (NumberVal u', NumberVal v') -> do
            push (NumberVal (u' + v'))
            exec code
        _ -> throwError "Plus non-nums"
exec (BOOLTRUE:code) = do
    push (BooleanVal True)
    exec code
exec (BOOLFALSE:code) = do
    push (BooleanVal False)
    exec code
exec (IF c a:code) = do
    v <- pop
    case v of
        BooleanVal b ->
            if b
                then exec (c ++ code)
                else exec (a ++ code)
        _ -> throwError "If non-boolean"
exec (LET n:code) = do
    v <- pop
    bind n v
    exec code
exec (FREE n:code) = do
    free n
    exec code
exec (VAR n:code) = do
    lookupVar n
    exec code
exec (DECLAREDNAME n:code) = do
    lookupDeclaredName n
    exec code