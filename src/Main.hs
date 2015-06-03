module Main where

import Statistic (runFold)
import qualified TextStatistic as TS
import Control.Applicative
import Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.ByteString.Lazy as B

data StatSuite = StatSuite {
    wordCount :: Int,
    lineCount :: Int,
    averageWordLength :: Float,
    mostCommonLetter :: Maybe (Char, Int)
} deriving (Show)

statSuite :: TS.TextStatistic StatSuite
statSuite = StatSuite <$> TS.wordCount <*> TS.lineCount <*> TS.averageWordLength <*> TS.mostCommonLetter

main :: IO ()
main = do
    content <- fmap E.decodeUtf8 B.getContents
    print $ runFold foldl' statSuite $ content
