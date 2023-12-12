#!/usr/bin/env stack
{- stack script
   --resolver lts-21.13
   --package array
-}

-- import           Data.Char (isAlpha)
-- import           Data.List (uncons, null)
-- import qualified Data.Map as Map
import           Debug.Trace (traceShowId)
import Data.Array (Array, array, bounds, assocs)
import Data.List (intercalate, sortOn)
import Data.Either (fromRight)
import Control.Monad (void)

data Cell = NS | EW | NE | NW | SW | SE | G | S
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
c2c 'S' = Right S
c2c c   = Left (c : " not a valid character")

-- type Sewer = Array (Int, Int) [Cell]

instance Show Cell where
  show NS = "|"
  show EW = "-"
  show NE = "┗"
  show NW = "┛"
  show SW = "┓"
  show SE = "┏"
  show G  = " "
  show S  = "S"

type Sewer = Array (Int, Int) Cell
instance {-# OVERLAPPING #-} Show Sewer where
  show arr = intercalate "" asStrs
    where
      ((_, _), (maxWidth, _)) = bounds arr
      byRows = sortOn (snd . fst) (assocs arr)
      insertNLAtEndOfRow ((x, _), v)= if x== maxWidth then show v ++ "\n" else show v
      asStrs = map insertNLAtEndOfRow byRows

matrixToArray m = array ((0, 0), (maxWidth, maxHeight)) [((x, y), (m !! y) !! x) | x <- [0..maxWidth], y <- [0..maxHeight]]
  where
    maxWidth = (length $ head m) - 1
    maxHeight = length m - 1
main = do
  s <- readFile "input"
  let parsedStrs = sequenceA $ map (sequenceA . map c2c) (lines s)
  print parsedStrs
  case parsedStrs of
        Right _ -> return ()
        Left msg -> print msg
  print $ fromRight (array ((0, 0), (0, 0)) []) (fmap matrixToArray parsedStrs)

  -- let width = length $ head parsedStrs
  -- let height = length parsedStrs
  -- let result = case parsedStrs of
        -- Right xs -> Right $ listMatrixToArray2D xs
        -- Left msg -> Left  msg
  
  -- print parsedStrs
  -- print result

  -- print $ mapM c2c "..SF7"
  -- print $ foldl (++) "" $ mapM (show . c2c) "..SF7"
