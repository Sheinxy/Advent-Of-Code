module Day05.Main (day05) where

import           AOC                (submit)
import           AOC.Utils          (isInRange)
import           Data.List.Split    (splitOn)
import qualified Data.RangeSet.List as RSet
import           GHC.Utils.Misc     (last2, count)

type Input = ([(Int, Int)], [Int])
type Output = Int

parseInput :: String -> Input
parseInput = (\[ranges, numbers] -> (map getRange ranges, map read numbers)) . splitOn [""] . lines
    where getRange = last2 . map read . splitOn "-"

partOne :: Input -> Output
partOne (ranges, numbers) = count (\x -> any (isInRange x) ranges) numbers

partTwo :: Input -> Output
partTwo = RSet.size . RSet.fromRangeList . fst

day05 :: String -> String -> IO ()
day05 "parse" = print . parseInput
day05 "one"   = print . partOne . parseInput
day05 "two"   = print . partTwo . parseInput
day05 "sone"  = submit 2025 5 1 . show . partOne . parseInput
day05 "stwo"  = submit 2025 5 2 . show . partTwo . parseInput
day05 _       = error "Undefined part"
