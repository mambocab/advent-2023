#!/usr/bin/env stack
{- stack script
   --resolver lts-21.13
   --package containers
   --package parsec
-}

import Data.Char (isDigit)
import Data.List (findIndices, mapAccumL, elemIndices)
import Data.Maybe (catMaybes)
import Data.Set (Set, disjoint, fromList, unions, member)
import Text.Read (readMaybe)

-- Part 1.
-- String to data structure utils.
data LineWindow = LineWindow {above :: String, line :: String, below :: String}
instance Show LineWindow where
    show lw = delim ++ "\n  " ++ above lw ++ "\n >" ++ line lw ++ "\n  " ++ below lw ++ "\n" ++ delim
      where
        delim = ['-' | _ <- [1 .. 2 + length (above lw)]]
windows :: [String] -> [LineWindow]
windows = windows' . preprocess
windows' :: [String] -> [LineWindow]
windows' (h0 : h1 : h2 : t) = newLW : windows' (h1 : h2 : t)
  where
    newLW = LineWindow{above = h0, line = h1, below = h2}
windows' _ = []
preprocess :: [String] -> [String]
preprocess [] = []
preprocess (h : t) = ['.' | _ <- [1 .. length h]] : h : preprocess' t
preprocess' [] = []
preprocess' [h] = [h, ['.' | _ <- [1 .. length h]]]
preprocess' (h : t) = h : preprocess' t

-- LineWindow parser.
data ParsedNumber = ParsedNumber {n :: Int, start :: Int, end :: Int} deriving (Show)
data ParserState = ParserState {pos :: Int, acc :: Maybe String} deriving (Show)

getParsedNumber (ParserState _ Nothing) = Nothing
getParsedNumber (ParserState pos (Just s)) = Just $ ParsedNumber (read s) (pos - length s) (pos - 1)

consumeChar :: ParserState -> Char -> (ParserState, Maybe ParsedNumber)
consumeChar ps@(ParserState pos Nothing) c = (ParserState (pos + 1) newState, Nothing)
  where
    newState
        | isDigit c = Just [c]
        | otherwise = Nothing
consumeChar ps@(ParserState pos (Just numSoFar)) c
    | isDigit c = (ParserState pos' (Just $ numSoFar ++ [c]), Nothing)
    -- Done with this digit; emit the parsed number.
    | otherwise = (ParserState pos' Nothing, getParsedNumber ps)
  where
    pos' = pos + 1

numsInLine :: String -> [ParsedNumber]
numsInLine s =
    let (finalState, parsed) = mapAccumL consumeChar (ParserState 0 Nothing) s
     in catMaybes $ parsed ++ [getParsedNumber finalState]

-- LineWindow checker
includedNums :: LineWindow -> [Int]
includedNums lw = map n $ filter shouldInclude (numsInLine $ line lw)
  where
    shouldInclude = include $ specialIndices lw
include :: Set Int -> ParsedNumber -> Bool
include indices (ParsedNumber _ s e) = not $ digitAdjacentCols `disjoint` indices
  where
    digitAdjacentCols = fromList [s - 1, s, s + 1, e - 1, e, e + 1]

specialIndices :: LineWindow -> Set Int
specialIndices lw = unions $ map specialIndices' $ sequenceA [above, line, below] lw
specialIndices' :: String -> Set Int
specialIndices' = fromList . findIndices specialChar
specialChar '.' = False
specialChar c = not $ isDigit c

part1 s = do
    let ws = windows $ lines s
    -- print $ head ws
    -- print $ map includedNums ws
    print $ sum $ concatMap includedNums ws

-- Part 2.
findGears lw = catMaybes $ map extractPairsOnly $ findGroupsAdjacentToAsterisk lw
  where
    extractPairsOnly (n0:n1:[]) = Just (n n0, n n1)
    extractPairsOnly _          = Nothing

findGearParsedPairs lw = filter (\x -> length x == 2) $ findGroupsAdjacentToAsterisk lw
findGroupsAdjacentToAsterisk lw = map (findAdjacentParsedNums lw) (gearCandidates lw)
  where
    gearCandidates = elemIndices '*' . line
findAdjacentParsedNums lw i = filter (adjacent i) pns
  where
    pns = concatMap numsInLine $ sequenceA [above, line, below] lw
    adjacent i pn = i `member` fromList [start pn - 1..end pn+1]

part2 s = do
    let ws = windows $ lines s
    -- print $ map findGearParsedPairs ws
    -- print $ map findGears ws
    -- print $ concatMap findGears ws
    -- print $ map (\(x, y) -> x * y) $ concatMap findGears ws
    print $ sum $ map (\(x, y) -> x * y) $ concatMap findGears ws

main = do
    example <- readFile "input/example.txt"
    input <- readFile "input/input.txt"

    putStrLn "part1"
    putStr "example: "
    part1 example
    putStr "input  : "
    part1 input

    putStrLn "\npart2"
    putStr "example: "
    part2 example
    putStr "input  : "
    part2 input
