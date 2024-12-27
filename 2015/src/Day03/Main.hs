module Day03.Main (day03) where

import           AOC              (submit)
import           Data.Function    ((&))
import           Data.List        (nub)
import           Data.Tuple.Extra (first, second)

type Input = [(Int, Int) -> (Int, Int)]
type Output = Int

parseInput :: String -> Input
parseInput = map getOp
    where getOp '>' = second ( 1 +)
          getOp '<' = second (-1 +)
          getOp 'v' = first  ( 1 +)
          getOp '^' = first  (-1 +)

partOne :: Input -> Output
partOne = length . nub . scanl (&) (0, 0)

partTwo :: Input -> Output
partTwo input = length . nub $ scanl (&) (0, 0) [x | (i, x) <- zip [0 .. ] input, even i] ++ scanl (&) (0, 0) [x | (i, x) <- zip [0 .. ] input, odd i]

day03 :: String -> String -> IO ()
day03 "parse" = print
day03 "one"   = print . partOne . parseInput
day03 "two"   = print . partTwo . parseInput
day03 "sone"  = submit 2015 3 1 . show . partOne . parseInput
day03 "stwo"  = submit 2015 3 2 . show . partTwo . parseInput
day03 _       = error "Undefined part"
