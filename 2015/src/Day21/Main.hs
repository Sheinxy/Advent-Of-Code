module Day21.Main (day21) where

import           AOC (submit)

type Input = String
type Output = Int

parseInput :: String -> Input
parseInput = undefined

partOne :: Input -> Output
partOne = undefined

partTwo :: Input -> Output
partTwo = undefined

day21 :: String -> String -> IO ()
day21 "parse" = print . parseInput
day21 "one"   = print . partOne . parseInput
day21 "two"   = print . partTwo . parseInput
day21 "sone"  = submit 2015 21 11 . show . partOne . parseInput
day21 "stwo"  = submit 2015 21 21 . show . partTwo . parseInput
day21 _       = error "Undefined part"
