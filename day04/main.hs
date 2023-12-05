#!/usr/bin/env stack
{- stack script
   --resolver lts-21.13
   --package parsec
   --package containers
-}

import           Data.Char  (isDigit)
import           Data.List  (sort, stripPrefix)
import qualified Data.Map   as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Set   as Set

parseLine s = Card sepWinners sepHave where
    cardless = fromMaybe s (stripPrefix "Card " s)
    (cardNoStr, rest) = span isDigit cardless
    (rawWinners, rawHave) = span (/= '|') rest
    splitHalf s = words $ dropWhile (not . isDigit) s
    [sepWinners, sepHave]= map splitHalf [rawWinners, rawHave]
    cardNo = read cardNoStr :: Int

matchCount card = Set.size $ Set.intersection (Set.fromList $ winners card) (Set.fromList $ numbers card)
score card = case matchCount card of
    0 -> 0
    n -> 2 ^ (n - 1)

data Card = Card { winners:: [String] , numbers :: [String] } deriving (Show)

data IntMultiset = IntMultiset (Map.Map Int Int) deriving Show
insertMany :: Int -> IntMultiset -> Int -> IntMultiset
insertMany addend (IntMultiset m) x = IntMultiset $ Map.alter inc x m
    where
        inc Nothing  = Just addend
        inc (Just n) = Just $ n + addend

insert ims x = insertMany 1 ims x

insertAll :: IntMultiset -> [Int] -> IntMultiset
insertAll ims = foldl insert ims

lookupIMS :: Int -> IntMultiset -> Int
lookupIMS k (IntMultiset m) = Map.findWithDefault 1 k m

unwrap (IntMultiset m) = m

copiesOfCards :: Map.Map Int Int -> IntMultiset
copiesOfCards matchCount = foldl (copiesOfCards' matchCount) init remainingKeys
    where
        init = (IntMultiset Map.empty) `insertAll` (Map.keys matchCount)
        remainingKeys = Map.keys matchCount  -- depends on Map.keys returning results sorted

copiesOfCards' :: Map.Map Int Int -> IntMultiset -> Int -> IntMultiset
copiesOfCards' matchCount acc i = foldl (insertMany cardsAtIdx) acc [i+1..i+matches]
    where
        cardsAtIdx = lookupIMS i acc
        matches = Map.findWithDefault 0 i matchCount


main = do
    -- part 1
    s <- readFile "example"
    let cardsEx = map parseLine $ lines s
    print $ sum $ map score cardsEx

    i <- readFile "input"
    print $ sum $ map (score . parseLine) $ lines i

    -- part 2
    -- example
    let matchCounts = Map.fromList $ zip [1..] $ map matchCount cardsEx
    print $ (unwrap . copiesOfCards) matchCounts
    print $ (sum . Map.elems . unwrap . copiesOfCards) matchCounts

    -- input
    let cardsInput = map parseLine $ lines i
    let matchCountsInput =  Map.fromList $ [1..] `zip` map matchCount cardsInput
    print $ (sum . Map.elems . unwrap . copiesOfCards) matchCountsInput

