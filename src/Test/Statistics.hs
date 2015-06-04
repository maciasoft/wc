module Test.Statistics where

import Prelude
import qualified Statistics as S
import Test.QuickCheck

runListFold :: S.Statistics t a -> [t] -> a
runListFold = S.runFold foldl

prop_count :: [Bool] -> Property
prop_count l = runListFold (S.count (True ==) 0) l === (length . filter (True ==)) l

main :: IO ()
main = do
    quickCheck prop_count