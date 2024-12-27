module Day23.Main (day23) where

import           AOC (submit)

type Input = String
type Output = Int

parseInput :: String -> Input
parseInput = undefined

partOne :: Input -> Output
partOne = undefined

partTwo :: Input -> Output
partTwo = undefined

day23 :: String -> String -> IO ()
day23 "parse" = print . parseInput
day23 "one"   = print . partOne . parseInput
day23 "two"   = print . partTwo . parseInput
day23 "sone"  = submit 2015 23 13 . show . partOne . parseInput
day23 "stwo"  = submit 2015 23 23 . show . partTwo . parseInput
day23 _       = error "Undefined part"
