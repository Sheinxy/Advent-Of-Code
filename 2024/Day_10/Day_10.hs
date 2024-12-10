module Main where

import System.Environment
import Data.Array.IArray
import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set

type Input = Array (Int, Int) Int
type Output = Int

parseInput :: String -> Input
parseInput input = listArray ((1, 1), (numRows, numCols)) (concat grid)
    where grid    = map (map digitToInt) $ lines input
          numRows = length grid
          numCols = length . head $ grid

getNeighbours :: Input -> (Int, Int) -> [(Int, Int)]
getNeighbours grid (i, j) = filter isInBound neighbours
    where ((minRow, minCol), (maxRow, maxCol)) = bounds grid
          isInBound (i, j) = minRow <= i && i <= maxRow && minCol <= j && j <= maxCol
          neighbours       = [(i + di, j + dj) | di <- [-1 .. 1], dj <- [-1 .. 1], abs di + abs dj == 1]

partOne :: Input -> Output
partOne input = sum [bfs [p] $ Set.singleton p | p <- startingPoints]
    where startingPoints = map fst . filter ((== 0) . snd) . assocs $ input
          bfs [] _ = 0
          bfs (x : xs) visited
            | current == 9   = 1 + bfs xs visited
            | otherwise      = bfs xs' visited'
            where neighbours = getNeighbours input x
                  neighbours'= filter (`Set.notMember` visited) neighbours
                  current    = input ! x
                  accessible = filter ((== current + 1) . (input !)) neighbours'
                  xs'        = xs ++ accessible
                  visited'   = foldr Set.insert visited accessible

partTwo :: Input -> Output
partTwo input = sum [bfs [p] | p <- startingPoints]
    where startingPoints = map fst . filter ((== 0) . snd) . assocs $ input
          bfs [] = 0
          bfs (x : xs) 
            | current == 9   = 1 + bfs xs
            | otherwise      = bfs xs'
            where neighbours = getNeighbours input x
                  current    = input ! x
                  accessible = filter ((== current + 1) . (input !)) neighbours
                  xs'        = xs ++ accessible

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args 
