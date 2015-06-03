module TextStatistics (
    TextStatistics,
    wordCount,
    lineCount,
    averageWordLength,
    mostCommonLetter
) where

import Statistics
import Control.Applicative ((<$>), (<*>))
import Data.HashMap.Strict
import Data.Char

-- Statistics on Char type

type TextStatistics a = Statistics Char a

-- predicates

isWordChar :: Char -> Bool
isWordChar = not <$> isSpace

isNewLine :: Char -> Bool
isNewLine c = c == '\n'

-- word count

data WordCountState = WordCountState Int Bool

wordCount' :: TextStatistics WordCountState
wordCount' = fold f (WordCountState 0 False)
    where
        f (WordCountState n isInWord) c = WordCountState (if not isInWord && isWordChar c then n + 1 else n) (isWordChar c)

wordCount :: TextStatistics Int
wordCount = f <$> wordCount'
    where
        f (WordCountState n _) = n

-- line count. Assuming empty file has 1 line of 0 length

lineCount :: TextStatistics Int
lineCount = count isNewLine 1

-- average word length

wordCharCount :: TextStatistics Int
wordCharCount = count isWordChar 0

averageWordLength :: TextStatistics Float
averageWordLength = (/) <$> (fmap fromIntegral wordCharCount) <*> (fmap fromIntegral wordCount)

-- most common character (if any) with number of usages

lettersMap :: TextStatistics (HashMap Char Int)
lettersMap = fold (\hm c -> if member c hm then insertWith (+) c 1 hm else insert c 1 hm) empty

maxChar :: Maybe (Char, Int) -> Char -> Int -> Maybe (Char, Int)
maxChar Nothing c n = Just (c, n)
maxChar (Just (c0, n0)) c n = if n > n0 then Just (c, n) else Just (c0, n0)

mostCommonLetter :: TextStatistics (Maybe (Char, Int))
mostCommonLetter = f <$> lettersMap
    where
        f = foldlWithKey' maxChar Nothing
