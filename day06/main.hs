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

-- part 1
parseLine1 = catMaybes . map readMaybeInt . words
parse1 s = map (uncurry Race) $ zip line1 line2
  where line1: line2: _ = map parseLine1 . lines $ s
numberOfRecordBreakers (Race meters record) = length $ filter (>record) $ possibleDistances meters
possibleDistances meters = [speed * (meters - speed) | speed <- [0..meters]]

-- part 2
parse s = Race (read metersStr) (read recordStr)
  where metersStr : recordStr : _ = map (filter isDigit) $ lines s

main = do
    s <- readFile "input"
    print $ product $ map numberOfRecordBreakers $ parse1 s
    print $ numberOfRecordBreakers $ parse s
