module Statistics (
    Statistics (value, consume),
    runFold,
    fold,
    count,
    filter
) where

import Prelude hiding (filter)
import Control.Applicative

-- Statistics type performs fold over streamed data of elements `t` and collects statistics of `a`.

data Statistics t a = Statistics {
    value :: a,
    consume :: (t -> Statistics t a)
}

--instances

instance Functor (Statistics t) where
    fmap f (Statistics a sa) = Statistics (f a) (\c -> fmap f (sa c))

instance Applicative (Statistics t) where
    pure a = Statistics a (\_ -> pure a)
    (Statistics f sf) <*> (Statistics a sa) = Statistics (f a) (\c -> (sf c) <*> (sa c))

-- run statistic engine using folding function

runFold :: ((Statistics t a -> t -> Statistics t a) -> Statistics t a -> ts -> Statistics t a) -> Statistics t a -> ts -> a
runFold f s ts = value $ f (\(Statistics _ sa) t -> sa t) s ts

-- factory functions

fold :: (a -> t -> a) -> a -> Statistics t a
fold f a = Statistics a (\t -> fold f (f a t))

filter :: (t -> Bool) -> Statistics t a -> Statistics t a
filter f (Statistics a sa) = Statistics a (\t -> filter f $ if f t then sa t else Statistics a sa)

count :: Integral a => (t -> Bool) -> a -> Statistics t a
count f = filter f . fold (\a _ -> a + 1)
