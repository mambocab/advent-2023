#!/usr/bin/env stack
{- stack script
   --resolver lts-21.13
   --package parsec
   --package containers
-}

import           Data.Char  (isDigit)
import           Data.List  (stripPrefix)
import           Data.Maybe (fromMaybe)
import           Data.Set   (fromList, intersection, size)

parseLine s = Card sepWinners sepHave where
    cardless = fromMaybe s (stripPrefix "Card " s)
    (cardNoStr, rest) = span isDigit cardless
    (rawWinners, rawHave) = span (/= '|') rest
    splitHalf s = words $ dropWhile (not . isDigit) s
    [sepWinners, sepHave]= map splitHalf [rawWinners, rawHave]
    cardNo = read cardNoStr :: Int

matchCount card = size $ intersection (fromList $ winners card) (fromList $ numbers card)
score card = case matchCount card of
    0 -> 0
    n -> 2 ^ (n - 1)

data Card = Card { winners:: [String] , numbers :: [String] } deriving (Show)

main = do
    -- part 1
    s <- readFile "example"
    let cardsP1 = map parseLine $ lines s
    putStr "scores: "
    print $ sum $ map score cardsP1

    i <- readFile "input"
    print $ sum $ map (score . parseLine) $ lines i
