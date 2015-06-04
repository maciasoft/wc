module Main (
    main,
    StatisticsSuite (..),
    runSuite
) where

import Prelude
import Statistics (runFold)
import qualified TextStatistics as TS
import Control.Applicative
import Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.ByteString.Lazy as B

data StatisticsSuite = StatisticsSuite {
    wordCount :: Int,
    lineCount :: Int,
    averageWordLength :: Maybe Float,
    mostCommonLetter :: Maybe Char
} deriving (Eq, Ord, Show)

statisticsSuite :: TS.TextStatistics StatisticsSuite
statisticsSuite = StatisticsSuite <$> TS.wordCount <*> TS.lineCount <*> TS.averageWordLength <*> TS.mostCommonLetter

runSuite :: T.Text -> StatisticsSuite
runSuite = runFold foldl' statisticsSuite

main :: IO ()
main = do
    content <- fmap E.decodeUtf8 B.getContents
    print $ runSuite $ content
