module Day18.Main (day18) where

import           AOC (submit)

type Input = String
type Output = Int

parseInput :: String -> Input
parseInput = undefined

partOne :: Input -> Output
partOne = undefined

partTwo :: Input -> Output
partTwo = undefined

day18 :: String -> String -> IO ()
day18 "parse" = print . parseInput
day18 "one"   = print . partOne . parseInput
day18 "two"   = print . partTwo . parseInput
day18 "sone"  = submit 2015 18 18 . show . partOne . parseInput
day18 "stwo"  = submit 2015 18 28 . show . partTwo . parseInput
day18 _       = error "Undefined part"
