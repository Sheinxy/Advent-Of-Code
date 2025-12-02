module Day02.Main (day02) where

import           AOC (submit)

type Input = String
type Output = Int

parseInput :: String -> Input
parseInput = undefined

partOne :: Input -> Output
partOne = undefined

partTwo :: Input -> Output
partTwo = undefined

day02 :: String -> String -> IO ()
day02 "parse" = print . parseInput
day02 "one"   = print . partOne . parseInput
day02 "two"   = print . partTwo . parseInput
day02 "sone"  = submit 2025 2 1 . show . partOne . parseInput
day02 "stwo"  = submit 2025 2 2 . show . partTwo . parseInput
day02 _       = error "Undefined part"
