module Day03.Main (day03) where

import           AOC (submit)

type Input = String
type Output = Int

parseInput :: String -> Input
parseInput = undefined

partOne :: Input -> Output
partOne = undefined

partTwo :: Input -> Output
partTwo = undefined

day03 :: String -> String -> IO ()
day03 "parse" = print . parseInput
day03 "one"   = print . partOne . parseInput
day03 "two"   = print . partTwo . parseInput
day03 "sone"  = submit 2025 3 1 . show . partOne . parseInput
day03 "stwo"  = submit 2025 3 2 . show . partTwo . parseInput
day03 _       = error "Undefined part"
