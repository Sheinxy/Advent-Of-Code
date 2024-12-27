module Day20.Main (day20) where

import           AOC (submit)

type Input = String
type Output = Int

parseInput :: String -> Input
parseInput = undefined

partOne :: Input -> Output
partOne = undefined

partTwo :: Input -> Output
partTwo = undefined

day20 :: String -> String -> IO ()
day20 "parse" = print . parseInput
day20 "one"   = print . partOne . parseInput
day20 "two"   = print . partTwo . parseInput
day20 "sone"  = submit 2015 20 10 . show . partOne . parseInput
day20 "stwo"  = submit 2015 20 20 . show . partTwo . parseInput
day20 _       = error "Undefined part"
