module Day08.Main (day08) where

import           AOC (submit)

type Input = String
type Output = Int

parseInput :: String -> Input
parseInput = undefined

partOne :: Input -> Output
partOne = undefined

partTwo :: Input -> Output
partTwo = undefined

day08 :: String -> String -> IO ()
day08 "parse" = print . parseInput
day08 "one"   = print . partOne . parseInput
day08 "two"   = print . partTwo . parseInput
day08 "sone"  = submit 2025 8 1 . show . partOne . parseInput
day08 "stwo"  = submit 2025 8 2 . show . partTwo . parseInput
day08 _       = error "Undefined part"
