module Day04.Main (day04) where

import           AOC (submit)

type Input = String
type Output = Int

parseInput :: String -> Input
parseInput = undefined

partOne :: Input -> Output
partOne = undefined

partTwo :: Input -> Output
partTwo = undefined

day04 :: String -> String -> IO ()
day04 "parse" = print . parseInput
day04 "one"   = print . partOne . parseInput
day04 "two"   = print . partTwo . parseInput
day04 "sone"  = submit 2025 4 1 . show . partOne . parseInput
day04 "stwo"  = submit 2025 4 2 . show . partTwo . parseInput
day04 _       = error "Undefined part"
