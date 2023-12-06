#!/usr/bin/env stack
{- stack script
   --resolver lts-21.13
   --package parsec
   --package containers
-}

import           Data.Char     (isDigit)
import           Data.Either   (rights)
import           Data.Foldable (asum)
import qualified Data.Map      as Map

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

hasSeedForLocation i almanac = case bizarroSeedForLocation i almanac of
    Right _ -> True
    Left _  -> False

bizarroSeedForLocation :: Int -> Almanac -> Either Int Int
bizarroSeedForLocation i (Almanac seeds maps)
    | seedVal `inSeedRanges` seeds = Right seedVal
    | otherwise = Left seedVal
        where seedVal = i `rLookupAlmanacMaps` maps

inSeedRanges i (x:y:tail) = (i >= x && i < x + y) || i `inSeedRanges` tail
inSeedRanges _ []         = False

-- Assumes 2nd argument is in Alamanac order.
rLookupAlmanacMaps :: Int -> [AlmanacMap] -> Int
rLookupAlmanacMaps = foldr $ flip rLookupAlmanacMap

rLookupAlmanacMap :: Int -> AlmanacMap -> Int
rLookupAlmanacMap i am = maybe i id $ asum $ map (rLookupMapEntry i) am
rLookupMapEntry :: Int -> MapEntry -> Maybe Int
rLookupMapEntry i (dstStart, srcStart, len)
    | i < dstStart = Nothing
    | offset < len = Just $ srcStart + offset
    | otherwise = Nothing
        where offset = i - dstStart

main = do
    s <- readFile "input"
    (print . minimum . translate . parseFile) s
    print $ head $ filter (flip hasSeedForLocation (parseFile s)) [0..]
