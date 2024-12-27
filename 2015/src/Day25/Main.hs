module Day25.Main (day25) where

import           AOC (submit)

type Input = String
type Output = Int

parseInput :: String -> Input
parseInput = undefined

partOne :: Input -> Output
partOne = undefined

partTwo :: Input -> Output
partTwo = undefined

day25 :: String -> String -> IO ()
day25 "parse" = print . parseInput
day25 "one"   = print . partOne . parseInput
day25 "two"   = print . partTwo . parseInput
day25 "sone"  = submit 2015 25 15 . show . partOne . parseInput
day25 "stwo"  = submit 2015 25 25 . show . partTwo . parseInput
day25 _       = error "Undefined part"
