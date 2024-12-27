module Day19.Main (day19) where

import           AOC (submit)

type Input = String
type Output = Int

parseInput :: String -> Input
parseInput = undefined

partOne :: Input -> Output
partOne = undefined

partTwo :: Input -> Output
partTwo = undefined

day19 :: String -> String -> IO ()
day19 "parse" = print . parseInput
day19 "one"   = print . partOne . parseInput
day19 "two"   = print . partTwo . parseInput
day19 "sone"  = submit 2015 19 19 . show . partOne . parseInput
day19 "stwo"  = submit 2015 19 29 . show . partTwo . parseInput
day19 _       = error "Undefined part"
