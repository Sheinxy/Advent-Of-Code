module Main where

import System.Environment

data Input  = Input  { galaxies :: [(Int, Int)], emptyRows :: [Int], emptyCols :: [Int] } deriving (Show)
type Output = Int

parseInput :: String -> Input
parseInput input = Input galaxies emptyRows emptyCols
    where rows      = lines input
          ncols     = length . head $ rows
          galaxies  = [(i, j) | (i, row) <- zip [1 .. ] rows, (j, char) <- zip [1 .. ] row, char == '#']
          emptyRows = [i      | (i, _)   <- zip [1 .. ] rows, i `notElem` map fst galaxies]
          emptyCols = [j      |  j       <-     [1 .. ncols], j `notElem` map snd galaxies]

getRealCoordinates :: Input -> Int -> [(Int, Int)]
getRealCoordinates (Input g r c) coef = map go g
    where go (row, col) = (row + (coef - 1) * rowsBefore, col + (coef - 1) * colsBefore)
            where rowsBefore = length . takeWhile (< row) $ r
                  colsBefore = length . takeWhile (< col) $ c

dist :: (Int, Int) -> (Int, Int) -> Int
dist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

solve :: Int -> Input -> Int
solve coef input = sum [dist a b | a <- coordinates, b <- coordinates, a < b]
    where coordinates = getRealCoordinates input coef

partOne :: Input -> Output
partOne = solve 2

partTwo :: Input -> Output
partTwo = solve 1000000

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args 
