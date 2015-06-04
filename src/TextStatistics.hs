module TextStatistics (
    TextStatistics,
    wordCount,
    lineCount,
    averageWordLength,
    mostCommonLetter
) where

import Prelude hiding (filter)
import qualified Statistics as S
import Control.Applicative ((<$>), (<*>))
import qualified Data.HashMap.Strict as HM
import Data.Char

-- Statistics on Char stream

type TextStatistics a = S.Statistics Char a

-- predicates

isWordChar :: Char -> Bool
isWordChar = not <$> isSpace

isNewLine :: Char -> Bool
isNewLine c = c == '\n'

-- word count

data WordCountState = WordCountState {
    charCount :: Int,
    isInWord :: Bool
}

wordCount :: TextStatistics Int
wordCount = charCount <$> S.fold f (WordCountState 0 False)
    where
        f w c = WordCountState (if not (isInWord w) && isWordChar c then (charCount w) + 1 else (charCount w)) (isWordChar c)

-- line count. Assuming empty file has 1 line of 0 length

lineCount :: TextStatistics Int
lineCount = S.count isNewLine 1

-- average word length

wordCharCount :: TextStatistics Int
wordCharCount = S.count isWordChar 0

averageWordLength :: TextStatistics (Maybe Float)
averageWordLength = maybeDivide <$> wordCharCount <*> wordCount
    where
        maybeDivide a b = if b /= 0 then Just (fromIntegral a / fromIntegral b) else Nothing

-- most common character (if any) with number of usages

mostCommonLetter :: TextStatistics (Maybe Char)
mostCommonLetter = fmap fst <$> ((HM.foldlWithKey' maxChar Nothing) <$> lettersMap)
    where
        lettersMap :: TextStatistics (HM.HashMap Char Int)
        lettersMap = S.filter isWordChar $ S.fold (\hm c -> if HM.member c hm then HM.insertWith (+) c 1 hm else HM.insert c 1 hm) HM.empty
        maxChar Nothing c n = Just (c, n)
        maxChar (Just (c0, n0)) c n = if n > n0 then Just (c, n) else Just (c0, n0)

