#!/usr/bin/env stack
{- stack script
   --resolver lts-21.13
   --package parsec
   --package containers
-}

import           Data.Char  (isDigit)
import           Data.Maybe (catMaybes)
import           Text.Read  (readMaybe)

readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe

data Race = Race { meters :: Int, recordMs :: Int } deriving Show
parseLine = catMaybes . map readMaybeInt . words
parse s = map (uncurry Race) $ zip line1 line2
  where line1: line2: _ = map parseLine . lines $ s

numberOfRecordBreakers (Race meters record) = length $ filter (>record) $ possibleDistances meters
possibleDistances meters = [speed * (meters - speed) | speed <- [0..meters]]

main = do
    s <- readFile "input"
    print $ product $ map numberOfRecordBreakers $ parse s
