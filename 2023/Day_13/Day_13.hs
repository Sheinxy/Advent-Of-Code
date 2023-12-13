module Main where

import Data.List
import Data.List.Split
import Data.Maybe (fromMaybe)
import System.Environment

type Input = [[String]]
type Output = Int

parseInput :: String -> Input
parseInput = map lines . splitOn "\n\n"

getMirrorDiffs :: String -> [Int]
getMirrorDiffs row = map (length . filter not) . zipWith (zipWith (==)) starts $ ends -- For each (left, right) pair, find the characters that differ and count the number of different characters
      where starts = (map reverse . tail . inits) row -- The different left  parts, mirrored
            ends   = (init        . tail . tails) row -- The different right parts

getMirrorRow :: Int -> [String] -> Int
getMirrorRow smudges row = 1 + fromMaybe (-1) (smudges `elemIndex` results) -- Find the row number that has the right number of smudges
           where results = map sum . (transpose . map getMirrorDiffs) $ row -- Number of smudges for each vertical line row number

solve :: Int -> Input -> Output
solve n grids = sum [getMirrorRow n g + 100 * (getMirrorRow n . transpose $ g) | g <- grids]

partOne :: Input -> Output
partOne = solve 0

partTwo :: Input -> Output
partTwo = solve 1

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args 
