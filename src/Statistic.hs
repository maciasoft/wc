{-# LANGUAGE MultiParamTypeClasses #-}
module Statistic (
    Statistic,
    runFold,
    fold,
    count
) where

import Control.Applicative

-- Stat type performs fold over streamed data and collects statistics.

data Statistic t a = Statistic a (t -> Statistic t a)

--instances

instance Functor (Statistic t) where
    fmap f (Statistic a s) = Statistic (f a) (\c -> fmap f (s c))

instance Applicative (Statistic t) where
    pure x = Statistic x (\_ -> pure x)
    (Statistic f sf) <*> (Statistic a sa) = Statistic (f a) (\c -> (sf c) <*> (sa c))

-- run statistic engine using folding function

runFold :: ((Statistic t a -> t -> Statistic t a) -> Statistic t a -> ts -> Statistic t a) -> Statistic t a -> ts -> a
runFold f s ts = a
    where
        Statistic a _ = f (\(Statistic _ sa) t -> sa t) s ts

-- factory functions

fold :: (a -> t -> a) -> a -> Statistic t a
fold f a = Statistic a (\t -> fold f (f a t))

count :: Integral a => (t -> Bool) -> a -> Statistic t a
count f = fold (\a t -> if f t then a + 1 else a)
