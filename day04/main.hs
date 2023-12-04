#!/usr/bin/env stack
{- stack script
   --resolver lts-21.13
   --package parsec
   --package containers
-}

import Data.Char (isDigit)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Set (fromList, size, intersection)

parseLine s = Card cardNo sepWinners sepHave where
    cardless = fromMaybe s (stripPrefix "Card " s)
    (cardNoStr, rest) = span isDigit cardless
    (rawWinners, rawHave) = span (/= '|') rest
    splitHalf s = words $ dropWhile (not . isDigit) s
    [sepWinners, sepHave]= map splitHalf [rawWinners, rawHave]
    cardNo = read cardNoStr :: Int

matchCount card = size $ intersection (fromList $ winners card) (fromList $ numbers card)
scoreCard card = case matchCount card of
    0 -> 0
    n -> 2 ^ (n - 1)

data Card = Card { cardNo :: Int , winners:: [String] , numbers :: [String] } deriving (Show)

main = do
    s <- readFile "example"
    let cards = map parseLine $ lines s
    mapM_ print cards
    print $ map matchCount cards
    print $ map ( (^ 2) .subtract 1 . matchCount) cards
    print $ map scoreCard cards
    print $ sum $ map scoreCard cards

    i <- readFile "input"
    print $ sum $ map (scoreCard . parseLine) $ lines i

