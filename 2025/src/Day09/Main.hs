module Day09.Main (day09) where

import           AOC (submit)

type Input = String
type Output = Int

parseInput :: String -> Input
parseInput = undefined

partOne :: Input -> Output
partOne = undefined

partTwo :: Input -> Output
partTwo = undefined

day09 :: String -> String -> IO ()
day09 "parse" = print . parseInput
day09 "one"   = print . partOne . parseInput
day09 "two"   = print . partTwo . parseInput
day09 "sone"  = submit 2025 9 1 . show . partOne . parseInput
day09 "stwo"  = submit 2025 9 2 . show . partTwo . parseInput
day09 _       = error "Undefined part"
