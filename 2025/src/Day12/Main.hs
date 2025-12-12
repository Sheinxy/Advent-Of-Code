module Day12.Main (day12) where

import           AOC             (submit)
import           Data.List.Split (splitOn)
import           GHC.Utils.Misc  (count)

type Present = [String]
data Region = Region { regionSize :: (Int, Int), regionPresents :: [Int] } deriving Show
data Input  = Input { presents :: [Present], regions :: [Region] } deriving Show

type Output = Int

parseInput :: String -> Input
parseInput raw = Input { presents = shapes', regions = regions' }
    where input = splitOn [""] . lines $ raw
          (shapes, regions) = (init input, last input)
          shapes' = map tail shapes
          regions' = map parseRegion regions
          parseRegion r = let (regSize, ':' : regPres) = span ( /= ':') r
                          in Region (parseSize regSize) (map read . words $ regPres)
          parseSize s = let (a, 'x' : b) = span ( /= 'x') s
                        in (read a, read b)

isTriviallyValid :: Region -> Bool
isTriviallyValid reg = numBlocks <= x' * y'
    where (x, y) = regionSize reg
          (x', y') = (x `div` 3, y `div` 3)
          numBlocks = sum . regionPresents $ reg

partOne :: Input -> Output
partOne = count isTriviallyValid . regions

partTwo :: Input -> String
partTwo = const "Merry Christmas! <3"

day12 :: String -> String -> IO ()
day12 "parse" = print . parseInput
day12 "one"   = print . partOne . parseInput
day12 "two"   = print . partTwo . parseInput
day12 "sone"  = submit 2025 12 1 . show . partOne . parseInput
day12 "stwo"  = submit 2025 12 2 . show . partTwo . parseInput
day12 _       = error "Undefined part"
