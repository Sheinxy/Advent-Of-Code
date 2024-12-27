module Day24.Main (day24) where

import           AOC (submit)

type Input = String
type Output = Int

parseInput :: String -> Input
parseInput = undefined

partOne :: Input -> Output
partOne = undefined

partTwo :: Input -> Output
partTwo = undefined

day24 :: String -> String -> IO ()
day24 "parse" = print . parseInput
day24 "one"   = print . partOne . parseInput
day24 "two"   = print . partTwo . parseInput
day24 "sone"  = submit 2015 24 14 . show . partOne . parseInput
day24 "stwo"  = submit 2015 24 24 . show . partTwo . parseInput
day24 _       = error "Undefined part"
