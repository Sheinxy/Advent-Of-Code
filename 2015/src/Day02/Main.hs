module Day02.Main (day02) where

import           AOC           (submit)
import           Data.Char
import           Data.Function
import           Data.List

type Input = [[Int]]
type Output = Int

parseInput :: String -> Input
parseInput = map (map read . filter (all isDigit) . groupBy ((==) `on` isDigit)) . lines

partOne :: Input -> Output
partOne input = sum [ 2 * l * w + 2 * w * h + 2 * h * l + smallest l w h | [l, w, h] <- input]
    where smallest l w h = minimum [l * w, w * h, l * h]

partTwo :: Input -> Output
partTwo input = sum [ l * w * h + 2 * sum (take 2 $ sort x) | x@[l, w, h] <- input]

day02 :: String -> String -> IO ()
day02 "parse" = print . parseInput
day02 "one"   = print . partOne . parseInput
day02 "two"   = print . partTwo . parseInput
day02 "sone"  = submit 2015 2 1 . show . partOne . parseInput
day02 "stwo"  = submit 2015 2 2 . show . partTwo . parseInput
day02 _       = error "Undefined part"
