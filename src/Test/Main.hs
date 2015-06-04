module Test.Main where

import Prelude
import Main (runSuite, StatisticsSuite (..))
import Test.QuickCheck
import Data.Text.Lazy (pack)

prop_runSuite_empty :: Bool
prop_runSuite_empty = runSuite (pack "") == StatisticsSuite {wordCount = 0, lineCount = 1, averageWordLength = Nothing, mostCommonLetter = Nothing}

prop_runSuite_wordCount :: String -> Property
prop_runSuite_wordCount s = wordCount (runSuite (pack s)) + 1 === wordCount (runSuite (pack (s ++ " anotherword")))

prop_runSuite_mostCommonLetter_ignoreSpace :: String -> Property
prop_runSuite_mostCommonLetter_ignoreSpace s = mostCommonLetter (runSuite (pack s)) === mostCommonLetter (runSuite (pack (s ++ " ")))

main :: IO ()
main = do
    quickCheck prop_runSuite_empty
    quickCheck prop_runSuite_wordCount
    quickCheck prop_runSuite_mostCommonLetter_ignoreSpace
