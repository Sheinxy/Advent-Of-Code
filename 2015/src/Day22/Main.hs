module Day22.Main (day22) where

import           AOC (submit)

type Input = String
type Output = Int

parseInput :: String -> Input
parseInput = undefined

partOne :: Input -> Output
partOne = undefined

partTwo :: Input -> Output
partTwo = undefined

day22 :: String -> String -> IO ()
day22 "parse" = print . parseInput
day22 "one"   = print . partOne . parseInput
day22 "two"   = print . partTwo . parseInput
day22 "sone"  = submit 2015 22 12 . show . partOne . parseInput
day22 "stwo"  = submit 2015 22 22 . show . partTwo . parseInput
day22 _       = error "Undefined part"
