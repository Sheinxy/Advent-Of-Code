module Main where

import System.Environment
import Data.List

type Input = [[Int]]
type Output = Int

parseInput :: String -> Input
parseInput = map (map read . words) . lines

partOne :: Input -> Output
partOne = length . filter isSafe . map computeDiff
    where computeDiff report = zipWith (-) report $ tail report
          isSafe diff = all (\x -> 1 <= x && x <= 3) diff || all (\x -> -3 <= x && x <= -1) diff

partTwo :: Input -> Output
partTwo = length . filter (any (isSafe . computeDiff) . computePossibilities)
    where computePossibilities l = filter (\x -> abs (length l - length x) <= 1) $ subsequences l
          computeDiff report = zipWith (-) report $ tail report
          isSafe diff = all (\x -> 1 <= x && x <= 3) diff || all (\x -> -3 <= x && x <= -1) diff

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args
