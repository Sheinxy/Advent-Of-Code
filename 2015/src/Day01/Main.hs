module Day01.Main (day01) where

import           AOC (submit)

type Input = [Int -> Int]
type Output = Int

parseInput :: String -> Input
parseInput = map getOp
    where getOp :: Char -> (Int -> Int)
          getOp '(' = (+ 1)
          getOp _   = subtract 1

partOne :: Input -> Output
partOne = foldr ($) 0

partTwo :: Input -> Output
partTwo = length . takeWhile (>= 0) . scanl (\a f -> f a) 0

day01 :: String -> String -> IO ()
day01 "parse" = print
day01 "one"   = print . partOne . parseInput
day01 "two"   = print . partTwo . parseInput
day01 "sone"  = submit 2015 1 1 . show . partOne . parseInput
day01 "stwo"  = submit 2015 1 2 . show . partTwo . parseInput
day01 _       = error "Undefined part"
