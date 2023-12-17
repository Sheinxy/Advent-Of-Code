module Main where

import Algorithm.Search (pruning, aStar)
import Data.Char (digitToInt)
import Data.Matrix (Matrix, fromLists, (!), nrows, ncols)
import Data.Maybe (fromJust)
import Data.Tuple.Extra (first, second)
import System.Environment

data Direction = North | South | East | West deriving (Show, Eq, Ord)
data Move = Move { position :: (Int, Int), direction :: Direction, consecutive :: Int } deriving (Show, Eq, Ord)

type Input = Matrix Int
type Output = Int

parseInput :: String -> Input
parseInput = fromLists . map (map digitToInt) . lines

opposite :: Direction -> Direction
opposite North = South
opposite South = North
opposite East  = West
opposite West  = East

addDirection :: Direction -> (Int, Int) -> (Int, Int)
addDirection North = first  (-1 +)
addDirection South = first  ( 1 +)
addDirection East  = second ( 1 +)
addDirection West  = second (-1 +)

getNeighbours :: Int -> Int -> Move -> [Move]
getNeighbours min max (Move pos dir consec) | mustKeepDirection = filter ((== dir) . direction) neighbours 
                                            | otherwise         = neighbours
                   where neighbours = filter ((<= max) . consecutive)                       . -- Do not try moves that would imply moving too much in one direction
                                      map (\x -> Move (addDirection x pos) x (newConsec x)) . -- Get the move state (position, direction and number of consecutive moves)
                                      filter (/= opposite dir) $ [North, South, East, West]   -- Try moves in all possible directions, excepting reverse

                         mustKeepDirection = 0 < consec && consec < min                       -- If we already move once in a direction, but not the minimum number of times, then we need to keep going

                         -- Getting the new number of consecutive moves for a move in the new direction
                         newConsec ndir | consec <= 0 = consec + 1 -- If there was no move yet, then this move is the first move (useful for the starting case)
                                        | ndir == dir = consec + 1 -- If we're still going in the same direction, then this is the consec + 1 move
                                        | otherwise   = 1          -- This is the first move in that direction

-- findMinHeatLoss (minimum number of moves) (maximum number of moves) (input grid) -> minimum heat loss
findMinHeatLoss :: Int -> Int -> Input -> Output
findMinHeatLoss min max grid = fst . fromJust . aStar getNexts getCost heuristic isTarget $ start
    where getNexts     = getNeighbours min max `pruning` (not . isInGrid) -- Get next possible states for a move state (ie. neighbours in grid that don't break the move rules)
          getCost _ ns = grid ! position ns                               -- Get the cost of moving to a neighbour     (ie. the value of the neighbour in the grid)
          isTarget m   = position m == end && consecutive m >= min        -- Is the current state our target state?    (ie. is it at the end of the grid, and did we fit the move rules to get there?)
          heuristic    = dist end . position                              -- Heuristic for aStar: The best path will be the shortest, weights not accounted.

          end        = (nrows grid, ncols grid) -- Our end goal
          start      = Move (1, 1) East 0       -- Our starting state (Tile (1, 1) with a random direction and 0 moves yet)

          isInGrid mv = 0 < row && row <= nrows grid && 0 < col && col <= ncols grid where (row, col) = position mv -- Is the Move state in the grid?
          dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2) -- Standard taxicab distance for heuristic

partOne :: Input -> Output
partOne = findMinHeatLoss 0 3

partTwo :: Input -> Output
partTwo = findMinHeatLoss 4 10

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args 
