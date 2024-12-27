module Day16.Main (day16) where

import           AOC (submit)

type Input = String
type Output = Int

parseInput :: String -> Input
parseInput = undefined

partOne :: Input -> Output
partOne = undefined

partTwo :: Input -> Output
partTwo = undefined

day16 :: String -> String -> IO ()
day16 "parse" = print . parseInput
day16 "one"   = print . partOne . parseInput
day16 "two"   = print . partTwo . parseInput
day16 "sone"  = submit 2015 16 16 . show . partOne . parseInput
day16 "stwo"  = submit 2015 16 26 . show . partTwo . parseInput
day16 _       = error "Undefined part"
