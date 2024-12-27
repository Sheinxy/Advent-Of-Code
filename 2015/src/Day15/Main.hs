module Day15.Main (day15) where

import           AOC (submit)

type Input = String
type Output = Int

parseInput :: String -> Input
parseInput = undefined

partOne :: Input -> Output
partOne = undefined

partTwo :: Input -> Output
partTwo = undefined

day15 :: String -> String -> IO ()
day15 "parse" = print . parseInput
day15 "one"   = print . partOne . parseInput
day15 "two"   = print . partTwo . parseInput
day15 "sone"  = submit 2015 15 15 . show . partOne . parseInput
day15 "stwo"  = submit 2015 15 25 . show . partTwo . parseInput
day15 _       = error "Undefined part"
