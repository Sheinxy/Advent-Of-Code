module Day17.Main (day17) where

import           AOC (submit)

type Input = String
type Output = Int

parseInput :: String -> Input
parseInput = undefined

partOne :: Input -> Output
partOne = undefined

partTwo :: Input -> Output
partTwo = undefined

day17 :: String -> String -> IO ()
day17 "parse" = print . parseInput
day17 "one"   = print . partOne . parseInput
day17 "two"   = print . partTwo . parseInput
day17 "sone"  = submit 2015 17 17 . show . partOne . parseInput
day17 "stwo"  = submit 2015 17 27 . show . partTwo . parseInput
day17 _       = error "Undefined part"
