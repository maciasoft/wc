module Statistics (
    Statistics,
    runFold,
    fold,
    count
) where

import Control.Applicative

-- Statistics type performs fold over streamed data of elements `t` and collects statistics of `a`.

data Statistics t a = Statistics a (t -> Statistics t a)

--instances

instance Functor (Statistics t) where
    fmap f (Statistics a s) = Statistics (f a) (\c -> fmap f (s c))

instance Applicative (Statistics t) where
    pure x = Statistics x (\_ -> pure x)
    (Statistics f sf) <*> (Statistics a sa) = Statistics (f a) (\c -> (sf c) <*> (sa c))

-- run statistic engine using folding function

runFold :: ((Statistics t a -> t -> Statistics t a) -> Statistics t a -> ts -> Statistics t a) -> Statistics t a -> ts -> a
runFold f s ts = a
    where
        Statistics a _ = f (\(Statistics _ sa) t -> sa t) s ts

-- factory functions

fold :: (a -> t -> a) -> a -> Statistics t a
fold f a = Statistics a (\t -> fold f (f a t))

count :: Integral a => (t -> Bool) -> a -> Statistics t a
count f = fold (\a t -> if f t then a + 1 else a)
