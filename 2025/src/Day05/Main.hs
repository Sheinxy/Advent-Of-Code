module Day05.Main (day05) where

import           AOC (submit)

type Input = String
type Output = Int

parseInput :: String -> Input
parseInput = undefined

partOne :: Input -> Output
partOne = undefined

partTwo :: Input -> Output
partTwo = undefined

day05 :: String -> String -> IO ()
day05 "parse" = print . parseInput
day05 "one"   = print . partOne . parseInput
day05 "two"   = print . partTwo . parseInput
day05 "sone"  = submit 2025 5 1 . show . partOne . parseInput
day05 "stwo"  = submit 2025 5 2 . show . partTwo . parseInput
day05 _       = error "Undefined part"
