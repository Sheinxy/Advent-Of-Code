module Day10.Main (day10) where

import           AOC (submit)

type Input = String
type Output = Int

parseInput :: String -> Input
parseInput = undefined

partOne :: Input -> Output
partOne = undefined

partTwo :: Input -> Output
partTwo = undefined

day10 :: String -> String -> IO ()
day10 "parse" = print . parseInput
day10 "one"   = print . partOne . parseInput
day10 "two"   = print . partTwo . parseInput
day10 "sone"  = submit 2025 10 1 . show . partOne . parseInput
day10 "stwo"  = submit 2025 10 2 . show . partTwo . parseInput
day10 _       = error "Undefined part"
