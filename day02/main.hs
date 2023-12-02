#!/usr/bin/env stack
{- stack script
   --resolver lts-21.13
   --package parsec
   --package containers
-}

import Control.Monad (void)
import Data.Map (Map, findWithDefault, fromList, insert, empty, elems)
import Text.Parsec (ParseError, anyChar, char, digit, endBy, endBy1, endOfLine, eof, letter, many, many1, noneOf, parse, sepBy, sepBy1, spaces, string)
import Text.Parsec.String (Parser)

colorPull = do
    spaces
    n <- many1 digit
    spaces
    c <- many1 letter
    return (n, c)

pull = colorPull `sepBy1` char ','

pulls = pull `sepBy1` char ';'

gamePair = do
    string "Game"
    spaces
    gameNo <- many1 digit
    spaces
    char ':'
    ps <- pulls
    return (gameNo, ps)

day2Input = gamePair `endBy1` endOfLine

parseInput = parse day2Input "(unknown)"

data Game = Game
    { gameNo :: Int
    , rounds :: [[Pull]]
    }
    deriving (Show)

toRounds :: [[(String, String)]] -> [[Pull]]
toRounds = map toRound

toRound :: [(String, String)] -> [Pull]
toRound = map toPull

toGames = map toGame

toGame (gameNoIn, roundsIn) = do
    let gameNo = read gameNoIn :: Int
    let rounds = toRounds roundsIn
    Game{gameNo = gameNo, rounds = rounds}

data Color = Red | Green | Blue | Unknown deriving (Eq, Ord, Show)

toColor "red" = Red
toColor "green" = Green
toColor "blue" = Blue
toColor _ = Unknown

data Pull = Pull
    { count :: Int
    , color :: Color
    }
    deriving (Show)

toPull :: (String, String) -> Pull
toPull (nIn, cIn) = do
    let n = read nIn :: Int
    let c = toColor cIn
    Pull{count = n, color = c}

type Bag = Map Color Int

possiblePull :: Bag -> Pull -> Bool
possiblePull bag pull =
    count pull <= findWithDefault 0 (color pull) bag

possibleRound bag = all (possiblePull bag)

possibleRounds bag = all (possibleRound bag)

possibleGame bag game = possibleRounds bag $ rounds game

testBag = fromList [(Red, 12), (Green, 13), (Blue, 14)] :: Bag

part1Test = possibleGame testBag

part1 path = do
    print path
    result <- readFile path
    case parseInput result of
        Left e -> error $ show e
        Right parsed -> do
            let serd = toGames parsed
            -- print serd
            let possibleGames = map gameNo $ filter part1Test serd
            print possibleGames
            print $ sum possibleGames
    putStr "\n"

getColor bag color = findWithDefault 0 color bag

accMinBag :: Bag -> Pull -> Bag
accMinBag bag pull =
    let old = getColor bag $ color pull
        new = max old $ count pull
    in insert (color pull) new bag

makeMinBag :: Game -> Bag
makeMinBag game = foldl accMinBag empty $ concat $ rounds game

part2 path = do
    print path
    result <- readFile path
    case parseInput result of
        Left e -> error $ show e
        Right parsed -> do
            let serd = toGames parsed
            print $ map (product . elems . makeMinBag) serd
            print $ sum $ map (product . elems . makeMinBag) serd

main = do
    part1 "./inputs/example.txt"
    part1 "./inputs/input.txt"

    part2 "./inputs/example.txt"
    part2 "./inputs/input.txt"
