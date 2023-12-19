module Main where

import Algorithm.Search (pruning, aStar)
import Data.Char (digitToInt)
import Data.Matrix (Matrix, fromLists, (!), nrows, ncols)
import Data.Maybe (fromJust)
import Data.List (nub)
import Data.Tuple.Extra (first, second)
import System.Environment

data Direction = North | South | East | West | None deriving (Show, Eq, Ord)
data Move = Move { position :: (Int, Int), direction :: Direction } deriving (Show, Eq, Ord)

type Input = Matrix Int
type Output = Int

parseInput :: String -> Input
parseInput = fromLists . map (map digitToInt) . lines

opposite :: Direction -> Direction
opposite North = South
opposite South = North
opposite East  = West
opposite West  = East
opposite None  = None

addDirection :: Int -> Direction -> (Int, Int) -> (Int, Int)
addDirection n North = first  (-n +)
addDirection n South = first  ( n +)
addDirection n East  = second ( n +)
addDirection n West  = second (-n +)

getNeighbours :: Int -> Int -> Move -> [Move]
getNeighbours min max (Move pos dir) = neighbours
                   where neighbours  = filter ((/= (1, 1)) . position)                                .
                                       concatMap (\d -> [Move (addDirection n d pos) d |
                                                         n <- [min .. max]             ])             .
                                       filter (/=          dir)                                       .
                                       filter (/= opposite dir) $ [North, South, East, West] 

findMinHeatLoss :: Int -> Int -> Input -> Output
findMinHeatLoss lo hi grid = fst . fromJust . aStar getNexts getCost heuristic isTarget $ start
    where getNexts     = getNeighbours lo hi `pruning` (not . isInGrid) 
          isTarget m   = position m == end
          heuristic    = dist end . position
          end        = (nrows grid, ncols grid)
          start      = Move (1, 1) None

          isInGrid mv = 0 < row && row <= nrows grid && 0 < col && col <= ncols grid where (row, col) = position mv
          dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

          getCost (Move (r1, c1) _) (Move (r2, c2) _) | r1 == r2  = sum $ map (grid !) [(r1, c) | c <- [min c1 c2 .. max c1 c2], c /= c1]
                                                      | otherwise = sum $ map (grid !) [(r, c1) | r <- [min r1 r2 .. max r1 r2], r /= r1]

partOne :: Input -> Output
partOne = findMinHeatLoss 1 3

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
