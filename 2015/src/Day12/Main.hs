module Day12.Main (day12) where

import           AOC (submit)

type Input = String
type Output = Int

parseInput :: String -> Input
parseInput = undefined

partOne :: Input -> Output
partOne = undefined

partTwo :: Input -> Output
partTwo = undefined

day12 :: String -> String -> IO ()
day12 "parse" = print . parseInput
day12 "one"   = print . partOne . parseInput
day12 "two"   = print . partTwo . parseInput
day12 "sone"  = submit 2015 12 12 . show . partOne . parseInput
day12 "stwo"  = submit 2015 12 22 . show . partTwo . parseInput
day12 _       = error "Undefined part"
