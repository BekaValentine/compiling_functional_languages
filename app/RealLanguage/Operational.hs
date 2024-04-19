{-# LANGUAGE GADTs #-}

module RealLanguage.Operational where

import Control.Monad (ap)

data Operational instr r where
    Ret :: r -> Operational instr r
    Instr :: instr a -> (a -> Operational instr r) -> Operational instr r

instance Functor (Operational instr) where
    fmap f (Ret x)     = Ret (f x)
    fmap f (Instr i k) = Instr i (fmap f . k)

instance Applicative (Operational instr) where
    pure  = Ret
    (<*>) = ap

instance Monad (Operational instr) where
    Ret x >>= f     = f x
    Instr i k >>= f = Instr i (\x -> k x >>= f)

exec :: instr a -> Operational instr a
exec i = Instr i Ret