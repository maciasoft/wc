module Main where

import Statistics (runFold)
import qualified TextStatistics as TS
import Control.Applicative
import Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.ByteString.Lazy as B

data StatisticsSuite = StatisticsSuite {
    wordCount :: Int,
    lineCount :: Int,
    averageWordLength :: Float,
    mostCommonLetter :: Maybe (Char, Int)
} deriving (Show)

statisticsSuite :: TS.TextStatistics StatisticsSuite
statisticsSuite = StatisticsSuite <$> TS.wordCount <*> TS.lineCount <*> TS.averageWordLength <*> TS.mostCommonLetter

main :: IO ()
main = do
    content <- fmap E.decodeUtf8 B.getContents
    print $ runFold foldl' statisticsSuite $ content
