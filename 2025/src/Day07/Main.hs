module Day07.Main (day07) where

import           AOC (submit)

type Input = String
type Output = Int

parseInput :: String -> Input
parseInput = undefined

partOne :: Input -> Output
partOne = undefined

partTwo :: Input -> Output
partTwo = undefined

day07 :: String -> String -> IO ()
day07 "parse" = print . parseInput
day07 "one"   = print . partOne . parseInput
day07 "two"   = print . partTwo . parseInput
day07 "sone"  = submit 2025 7 1 . show . partOne . parseInput
day07 "stwo"  = submit 2025 7 2 . show . partTwo . parseInput
day07 _       = error "Undefined part"
