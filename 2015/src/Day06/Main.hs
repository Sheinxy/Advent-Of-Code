module Day06.Main (day06) where

import           AOC (submit)

type Input = String
type Output = Int

parseInput :: String -> Input
parseInput = undefined

partOne :: Input -> Output
partOne = undefined

partTwo :: Input -> Output
partTwo = undefined

day06 :: String -> String -> IO ()
day06 "parse" = print . parseInput
day06 "one"   = print . partOne . parseInput
day06 "two"   = print . partTwo . parseInput
day06 "sone"  = submit 2015 6 1 . show . partOne . parseInput
day06 "stwo"  = submit 2015 6 2 . show . partTwo . parseInput
day06 _       = error "Undefined part"
