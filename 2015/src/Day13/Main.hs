module Day13.Main (day13) where

import           AOC (submit)

type Input = String
type Output = Int

parseInput :: String -> Input
parseInput = undefined

partOne :: Input -> Output
partOne = undefined

partTwo :: Input -> Output
partTwo = undefined

day13 :: String -> String -> IO ()
day13 "parse" = print . parseInput
day13 "one"   = print . partOne . parseInput
day13 "two"   = print . partTwo . parseInput
day13 "sone"  = submit 2015 13 13 . show . partOne . parseInput
day13 "stwo"  = submit 2015 13 23 . show . partTwo . parseInput
day13 _       = error "Undefined part"
