#!/usr/bin/env stack
{- stack script
   --resolver lts-21.13
   --package array
   --package containers
-}

-- import           Data.Char (isAlpha)
-- import           Data.List (uncons, null)
-- import qualified Data.Map as Map
import           Debug.Trace (traceShowId, trace, traceShow)
import Data.Array.IArray (Array, array, bounds, assocs, (!), (//), indices, amap)
import Data.Ix (inRange)
import Data.List (intercalate, sortOn)
import Data.Either (fromRight)
import Control.Monad (void)
import Data.Maybe (listToMaybe, maybeToList)
import qualified Data.Set as Set

data Cell = NS | EW | NE | NW | SW | SE | G | St deriving Eq
-- | is a vertical pipe connecting north and south.
c2c '|' = Right NS
-- - is a horizontal pipe connecting east and west.
c2c '-' = Right EW
-- L is a 90-degree bend connecting north and east.
c2c 'L' = Right NE
-- J is a 90-degree bend connecting north and west.
c2c 'J' = Right NW
-- 7 is a 90-degree bend connecting south and west.
c2c '7' = Right SW
-- F is a 90-degree bend connecting south and east.
c2c 'F' = Right SE
-- . is ground; there is no pipe in this tile.
c2c '.' = Right G
-- S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.
c2c 'S' = Right St
c2c c   = Left (c : " not a valid character")

instance Show Cell where
  show NS = "|"
  show EW = "-"
  show NE = "┗"
  show NW = "┛"
  show SW = "┓"
  show SE = "┏"
  show G  = " "
  show St = "S"

data Direction = N | S | E | W deriving Show

hasConnector St _ = True  -- Start connects with anything adjacent and facing it.
hasConnector G  _ = False -- Ground connects with nothing.
hasConnector NS N = True
hasConnector NS S = True
hasConnector EW E = True
hasConnector EW W = True
hasConnector NE N = True
hasConnector NE E = True
hasConnector NW W = True
hasConnector NW N = True
hasConnector SW S = True
hasConnector SW W = True
hasConnector SE S = True
hasConnector SE E = True
hasConnector _  _ = False
opposite E = W
opposite W = E
opposite S = N
opposite N = S

type Sewer = Array (Int, Int) Cell
instance {-# OVERLAPPING #-} Show Sewer where
  show arr = intercalate "" asStrs
    where
      ((_, _), (maxWidth, _)) = bounds arr
      byRows = sortOn (snd . fst) (assocs arr)
      insertNLAtEndOfRow ((x, _), v)= if x== maxWidth then show v ++ "\n" else show v
      asStrs = map insertNLAtEndOfRow byRows

indexOf i arr = listToMaybe [j | (j, e) <- assocs arr, e == i]

directionFrom:: (Int, Int) -> (Int, Int) -> Maybe Direction
directionFrom (x, y) (x', y')
  | (x', y') == (x + 1, y) = Just E
  | (x', y') == (x - 1, y) = Just W
  | (x', y') == (x, y + 1) = Just S
  | (x', y') == (x, y - 1) = Just N
  | otherwise = Nothing

cardinalNeighbors (x, y) = Set.fromList [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

connected :: ((Int, Int), Cell) -> ((Int, Int), Cell) -> Bool
connected (coord, cell) (coord', cell') = case directionFrom coord coord' of
  Nothing  -> False
  Just dir -> hasConnector cell dir && hasConnector cell' (opposite dir)

connectedInSewer :: Sewer -> (Int, Int) -> (Int, Int) -> Bool
connectedInSewer sewer left right
  | left `notElem` indices sewer  = False
  | right `notElem` indices sewer = False
  | otherwise = connected (left, leftE) (right, rightE)
    where
      leftE = sewer ! left
      rightE = sewer ! right

connectionsInSewer :: Sewer -> (Int, Int) -> Set.Set (Int, Int)
connectionsInSewer sewer point = Set.filter (connectedInSewer sewer point) (cardinalNeighbors point)


findLoop :: Sewer -> Set.Set (Int, Int)
findLoop sewer = findLoop' (Set.fromList $ maybeToList $ indexOf St sewer) Set.empty sewer
findLoop' :: Set.Set (Int, Int) -> Set.Set (Int, Int) -> Sewer -> Set.Set (Int, Int)
findLoop' frontier resultsAcc sewer
  | Set.null frontier = resultsAcc
  | otherwise = findLoop' frontier' resultsAcc' sewer
      where
        adjs        = Set.unions $ Set.map (connectionsInSewer sewer) frontier
        frontier'   = Set.difference adjs resultsAcc
        resultsAcc' = frontier `Set.union` resultsAcc

matrixToArray m = array ((0, 0), (maxWidth, maxHeight)) [((x, y), (m !! y) !! x) | x <- [0..maxWidth], y <- [0..maxHeight]]
  where
    maxWidth = (length $ head m) - 1
    maxHeight = length m - 1

main = do
  s <- readFile "input"
  let parsedStrs = sequenceA $ map (sequenceA . map c2c) (lines s)
  -- print parsedStrs
  case parsedStrs of
        Right _ -> return ()
        Left msg -> print msg
  let sewer :: Sewer = fromRight (array ((0, 0), (0, 0)) []) (fmap matrixToArray parsedStrs)
  -- print sewer
  
  let maybeStartIndex = indexOf St sewer
  let nextIndices = case maybeStartIndex of
        Nothing -> []
        Just startIndex -> Set.toList $ connectionsInSewer sewer startIndex

  let loopIndices = findLoop sewer

  let outsideLoop = [(i, G) | (i, _) <- filter ((`Set.notMember` loopIndices) . fst) $ assocs sewer]
  let pruned = sewer // outsideLoop
  print pruned
