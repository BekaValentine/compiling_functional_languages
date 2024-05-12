{-# LANGUAGE GADTs, MultiParamTypeClasses, RankNTypes, TypeSynonymInstances, FlexibleInstances #-}

module ProofDevelopment.HuttonsRazor where

import Control.Monad (ap)
import Control.Monad.Writer


main :: IO ()
main = do
    print (eval ex)
    print (exec $ Entering [] ex)
    print (execO runRazor (translate ex))
    print (execM (Instr (EVAL ex) Ret) :: Writer [Term] Int)

ex = Plus (Lit 1) (Lit 2)

data Term
    = Lit Int
    | Plus Term Term
    deriving (Show)

eval :: Term -> Int
eval (Lit i) = i
eval (Plus x y) = eval x + eval y

data Frame
    = PlusL Term
    | PlusR Int

data Machine
    = Halt Int
    | Entering [Frame] Term
    | Exiting [Frame] Int

exec :: Machine -> Int
exec (Halt i) = i
exec (Entering k (Lit i)) =
    exec (Exiting k i)
exec (Entering k (Plus x y)) =
    exec (Entering (PlusL y:k) x)
exec (Exiting (PlusL y:k) i) =
    exec (Entering (PlusR i:k) y)
exec (Exiting (PlusR i:k) j) =
    exec (Exiting k (i+j))
exec (Exiting [] i) =
    exec (Halt i)



data Operational instr r where
    Ret :: r -> Operational instr r
    Instr :: instr a -> (a -> Operational instr r) -> Operational instr r

-- instance Functor (Operational instr) where
--     fmap f (Ret x)     = Ret (f x)
--     fmap f (Instr i k) = Instr i (fmap f . k)

-- instance Applicative (Operational instr) where
--     pure  = Ret
--     (<*>) = ap

-- instance Monad (Operational instr) where
--     Ret x >>= f     = f x
--     Instr i k >>= f = Instr i (\x -> k x >>= f)

-- -- exec :: instr a -> Operational instr a
-- -- exec i = Instr i Ret

execO :: Monad m => (forall a. instr a -> m a) -> Operational instr r -> m r
execO runI (Ret v) = return v
execO runI (Instr i k) =
    runI i >>= \v -> execO runI (k v)

class Decomposable instr m where
    decompose :: instr a -> m (Operational instr a)

execM :: (Monad m, Decomposable instr m)
      => Operational instr r -> m r
execM (Ret v) = return v
execM (Instr i k) =
    do
        p <- decompose i
        v <- execM p
        execM (k v)

data RazorI r where
    LIT :: Int -> RazorI Int
    PLUS :: Int -> Int -> RazorI Int

-- Operational RazorI Int -> (Int -> Operational RazorI Int) -> Operational RazorI Int

translateOnto :: Term -> (Int -> Operational RazorI Int) -> Operational RazorI Int
translateOnto (Lit i) k = Instr (LIT i) k
translateOnto (Plus x y) k =
    translateOnto x (\i ->
        translateOnto y (\j ->
            Instr (PLUS i j) k))

translate x = translateOnto x Ret

-- translate (Plus (Lit 1) (Lit 2))
-- = Instr (LIT 1) \i ->
--     Instr (LIT 2) \j ->
--      Instr (PLUS i j) \k ->
--          Ret k

newtype Id a = Id a
    deriving (Show)

instance Functor Id where
    fmap f (Id x) = Id (f x)

instance Applicative Id where
    pure = Id
    Id f <*> Id x = Id (f x)

instance Monad Id where
    Id x >>= k = k x 

runRazor :: forall a. RazorI a -> Id a
runRazor (LIT i) = Id i
runRazor (PLUS i j) = Id (i + j)


data EvalI r where
    EVAL :: Term -> EvalI Int

instance Decomposable EvalI (Writer [Term]) where
    decompose (EVAL (Lit i)) =
        do
            tell [Lit i]
            return (Ret i)
    decompose (EVAL (Plus m n)) =
        do
            tell [Plus m n]
            return (Instr (EVAL m) (\i -> Instr (EVAL n) (\j -> Ret (i + j))))