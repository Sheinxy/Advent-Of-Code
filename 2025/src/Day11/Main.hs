module Day11.Main (day11) where

import           AOC (submit)

type Input = String
type Output = Int

parseInput :: String -> Input
parseInput = undefined

partOne :: Input -> Output
partOne = undefined

partTwo :: Input -> Output
partTwo = undefined

day11 :: String -> String -> IO ()
day11 "parse" = print . parseInput
day11 "one"   = print . partOne . parseInput
day11 "two"   = print . partTwo . parseInput
day11 "sone"  = submit 2025 11 1 . show . partOne . parseInput
day11 "stwo"  = submit 2025 11 2 . show . partTwo . parseInput
day11 _       = error "Undefined part"
