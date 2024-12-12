module Main where

import System.Environment
import Data.Array.IArray
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

type Input = Array (Int, Int) Char
type Output = Int

parseInput :: String -> Input
parseInput input = listArray ((1, 1), (numRows, numCols)) (concat grid)
    where grid    = lines input
          numRows = length grid
          numCols = length . head $ grid

getNeighbours :: Input -> (Int, Int) -> [(Int, Int)]
getNeighbours grid (i, j) = filter isInBound neighbours
    where ((minRow, minCol), (maxRow, maxCol)) = bounds grid
          isInBound (i, j) = minRow <= i && i <= maxRow && minCol <= j && j <= maxCol
          neighbours       = [(i + di, j + dj) | di <- [-1 .. 1], dj <- [-1 .. 1], abs di + abs dj == 1]

findRegions :: Input -> [Set (Int, Int)]
findRegions input = foldr visit [] (indices input)
    where visit idx vs
            | any (Set.member idx) vs  = vs
            | otherwise                = bfs [idx] (Set.singleton idx) : vs
          bfs [] v = v
          bfs (x : xs) visited = bfs xs' visited'
            where neighbours = getNeighbours input x
                  neighbours'= filter (`Set.notMember` visited) neighbours
                  current    = input ! x
                  accessible = filter ((== current) . (input !)) neighbours'
                  xs'        = xs ++ accessible
                  visited'   = foldr Set.insert visited accessible

computePrice :: Set (Int, Int) -> Int
computePrice region = Set.size region * Set.foldr (\x acc -> acc + getBorderSize x) 0 region
    where getBorderSize (i, j) = length [(i + di, j + dj) | di <- [-1 .. 1],
                                                            dj <- [-1 .. 1],
                                                            abs di + abs dj == 1,
                                                            (i + di, j + dj) `Set.notMember` region]

partOne :: Input -> Output
partOne = sum . map computePrice . findRegions

computeBulkPrice :: Set (Int, Int) -> Int
computeBulkPrice region = sum (map countEdgesOfTurn turnPoints) * Set.size region
    where -- These are the corners (L7FJ) of the shape.
          turnPoints = Set.toAscList $ Set.filter isTurnPoint region

          -- This tests if a point is a corner. F and J are outer corners, while L and 7 are inner corners.
          -- This is done in order to count each edge only once.
          isOutFTurnPoint (i, j) = all (`Set.notMember` region) [(i - 1, j), (i, j - 1)]
          isOutJTurnPoint (i, j) = all (`Set.notMember` region) [(i + 1, j), (i, j + 1)]
          isInLTurnPoint  (i, j) = (i - 1, j + 1) `Set.notMember` region && all (`Set.member` region) [(i - 1, j), (i, j + 1)]
          isIn7TurnPoint  (i, j) = (i + 1, j - 1) `Set.notMember` region && all (`Set.member` region) [(i + 1, j), (i, j - 1)]
          turnPointList          = [isOutFTurnPoint, isOutJTurnPoint, isInLTurnPoint, isIn7TurnPoint]

          isTurnPoint x          = any ($ x) turnPointList
          countEdgesOfTurn x     = 2 * length (filter ($ x) turnPointList)

partTwo :: Input -> Output
partTwo = sum . map computeBulkPrice . findRegions

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args 
