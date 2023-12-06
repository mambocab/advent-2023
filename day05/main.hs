#!/usr/bin/env stack
{- stack script
   --resolver lts-21.13
   --package parsec
   --package containers
-}

import           Data.Char     (isDigit)
import           Data.Foldable (asum)
import qualified Data.Map      as Map
import qualified Debug.Trace   as Trace

data Almanac = Almanac { seeds :: [Int], maps :: [AlmanacMap] } deriving Show
type MapEntry = (Int, Int, Int)
type AlmanacMap = [MapEntry]

parseFile s = Almanac seeds maps
    where
        seedsTail = seekDigit s
        (rawSeeds, seedToSoilTail) = break (== '\n') seedsTail
        seeds = parseIntLine rawSeeds
        remainingLines = filter (/= "") $ lines $ seekDigit seedToSoilTail
        rawMaps = splitOnCond (not . startsWithDigit) remainingLines
        maps = map (map parseMapLine) rawMaps

-- stolen from words impl
splitOnCond :: (a -> Bool) -> [a] -> [[a]]
splitOnCond condition xs = case dropWhile condition xs of
    [] -> []
    xs' -> h : splitOnCond condition xs''
        where (h, xs'') = break condition xs'

seekDigit = dropWhile (not . isDigit)
startsWithDigit []    = False
startsWithDigit (x:_) = isDigit x
parseIntLine :: String -> [Int]
parseIntLine = map read . words
parseMapLine :: String -> MapEntry
parseMapLine s = (i0, i1, i2)
    where
        [i0, i1, i2] = parseIntLine s

translate :: Almanac -> [Int]
translate (Almanac seeds (headMap:tailMaps)) = translate $ Almanac seeds' tailMaps
    where
        seeds' = map (`trAlmanacMap` headMap) seeds
translate (Almanac seeds []) = seeds

trAlmanacMap :: Int -> AlmanacMap -> Int
trAlmanacMap i am = maybe i id $ asum $ map (trMapEntry i) am
trMapEntry :: Int -> MapEntry -> Maybe Int
trMapEntry i (dstStart, srcStart, len)
    | i >= srcStart && offset < len = Just (dstStart + offset)
    | otherwise = Nothing
        where offset = i - srcStart

main = (print . minimum . translate . parseFile) =<< readFile "input"
