module Day05.Main (day05) where

import           AOC                (submit)
import           AOC.Utils          (between, groupByNT, isInRange)
import           Data.List          (maximum, minimum, sort)
import           Data.List.Split    (splitOn)
import qualified Data.RangeSet.List as RSet
import           GHC.Utils.Misc     (count, last2)

type Input = ([(Int, Int)], [Int])
type Output = Int

parseInput :: String -> Input
parseInput = (\[ranges, numbers] -> (map getRange ranges, map read numbers)) . splitOn [""] . lines
    where getRange = last2 . map read . splitOn "-"

partOne :: Input -> Output
partOne (ranges, numbers) = count (\x -> any (isInRange x) ranges) numbers

partTwo :: Input -> Output
partTwo = RSet.size . RSet.fromRangeList . fst

unionize :: [(Int, Int)] -> [(Int, Int)]
unionize = map (\l -> (minimum . map fst $ l, maximum . map snd $ l))
         . groupByNT intersects . sort
    where intersects (a, b) (c, d) = (a `between` c $ d) || (c `between` a $ b)

partTwo' :: Input -> Output
partTwo' = sum . map (\(a, b) -> b - a + 1) . unionize . fst

day05 :: String -> String -> IO ()
day05 "parse" = print . parseInput
day05 "one"   = print . partOne . parseInput
day05 "two"   = print . partTwo . parseInput
day05 "two'"  = print . partTwo' . parseInput
day05 "sone"  = submit 2025 5 1 . show . partOne . parseInput
day05 "stwo"  = submit 2025 5 2 . show . partTwo . parseInput
day05 _       = error "Undefined part"
