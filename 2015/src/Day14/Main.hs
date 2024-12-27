module Day14.Main (day14) where

import           AOC (submit)

type Input = String
type Output = Int

parseInput :: String -> Input
parseInput = undefined

partOne :: Input -> Output
partOne = undefined

partTwo :: Input -> Output
partTwo = undefined

day14 :: String -> String -> IO ()
day14 "parse" = print . parseInput
day14 "one"   = print . partOne . parseInput
day14 "two"   = print . partTwo . parseInput
day14 "sone"  = submit 2015 14 14 . show . partOne . parseInput
day14 "stwo"  = submit 2015 14 24 . show . partTwo . parseInput
day14 _       = error "Undefined part"
