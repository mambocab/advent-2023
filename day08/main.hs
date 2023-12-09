#!/usr/bin/env stack
{- stack script
   --resolver lts-21.13
   --package parsec
   --package containers
-}

import           Data.Char (isAlpha)
import           Data.List (uncons, null)
import qualified Data.Map as Map
import Debug.Trace (traceShowId)

parseNodeLines :: [String] -> Map.Map String (String, String)
parseNodeLines xs = Map.unions (map parseNodeLine xs)
parseNodeLine :: String -> Map.Map String (String, String)
parseNodeLine s = case filter isAlpha s of
  (a:b:c:d:e:f:g:h:i:_) -> Map.insert [a, b, c] ([d, e, f], [g, h, i]) Map.empty
  _ -> Map.empty

parseFile s = case uncons $ (filter (not . null) . lines) s of
  Nothing                 -> DesertMap "" Map.empty
  Just (instrs, rawNodes) -> DesertMap instrs (parseNodeLines rawNodes)

data DesertMap = DesertMap { instructions :: String, nodes :: Map.Map String (String, String)} deriving Show

-- inclusive takeWhile.
takeWhileIncl p [] = []
takeWhileIncl p (x:xs)
  | p x = x : takeWhileIncl p xs
  | otherwise = [x]

data DesertTrek = DesertTrek {dMap :: DesertMap, state :: (Int, String)}
nextInstructionIndex (DesertTrek _ (i, _)) = i
location (DesertTrek _ (_, loc)) = loc

step (DesertTrek dm (nextInd, loc)) = DesertTrek dm (nextInd', loc')
  where
    nextInd' = (nextInd + 1) `mod` (length $ instructions dm)
    getter = if ('L' == instructions dm !! nextInd) then fst else snd
    loc' = getter $ nodes dm Map.! loc
steps dt = tail [location x | x <- iterate step dt]

main = do
  s <- readFile "input"
  let parsed = parseFile s
  -- print $ parsed
  let locs = steps (DesertTrek parsed (0, "AAA"))
  print $ length $ takeWhileIncl (/= "ZZZ") locs
