module Day11.Main (day11) where

import           AOC                   (submit)
import           Data.Function.Memoize (memoFix)
import           Data.Map              ((!))
import qualified Data.Map              as M

type Input = M.Map String [String]
type Output = Int

parseInput :: String -> Input
parseInput = M.fromList . map parseLine . lines
    where parseLine raw = let (key, ':' : rest) = break (== ':') raw in (key, words rest)

countPathFromTo :: String -> String -> Input -> Output
countPathFromTo from to input = memoFix go from
    where go f key | key == to    = 1
                   | key == "out" = 0
                   | otherwise = sum [f next | next <- input ! key]

partOne :: Input -> Output
partOne = countPathFromTo "you" "out"

partTwo :: Input -> Output
partTwo input | fftToDac /= 0 && dacToFft /= 0 = error "Something went wrong. Please check your input. (There can't be both a path from fft to dac and from dac to fft)"
              | fftToDac == 0 = countPathFromTo "svr" "dac" input * dacToFft * countPathFromTo "fft" "out" input
              | otherwise = countPathFromTo "svr" "fft" input * fftToDac * countPathFromTo "dac" "out" input
    where fftToDac = countPathFromTo "fft" "dac" input
          dacToFft = countPathFromTo "dac" "fft" input


day11 :: String -> String -> IO ()
day11 "parse" = print . parseInput
day11 "one"   = print . partOne . parseInput
day11 "two"   = print . partTwo . parseInput
day11 "sone"  = submit 2025 11 1 . show . partOne . parseInput
day11 "stwo"  = submit 2025 11 2 . show . partTwo . parseInput
day11 _       = error "Undefined part"
