module Day03.Main (day03) where

import           AOC       (submit)
import           Data.List (maximumBy)
import           Data.Ord  (comparing)

type Input = [String]
type Output = Int

parseInput :: String -> Input
parseInput = lines

findJoltage :: Int -> String -> Int
findJoltage numBat = read . step numBat
    where step 0 _ = []
          step n battery = first : next
            where (first, idx) = maximumBy (comparing fst) . drop (n - 1) . reverse $ zip battery [1 .. ]
                  next = step (n - 1) . drop idx $ battery

partOne :: Input -> Output
partOne = sum . map (findJoltage 2)

partTwo :: Input -> Output
partTwo = sum . map (findJoltage 12)

day03 :: String -> String -> IO ()
day03 "parse" = print . parseInput
day03 "one"   = print . partOne . parseInput
day03 "two"   = print . partTwo . parseInput
day03 "sone"  = submit 2025 3 1 . show . partOne . parseInput
day03 "stwo"  = submit 2025 3 2 . show . partTwo . parseInput
day03 _       = error "Undefined part"
