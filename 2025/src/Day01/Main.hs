module Day01.Main (day01) where

import           AOC            (submit)
import           GHC.Utils.Misc (count)

type Input = [Int]
type Output = Int

parseInput :: String -> Input
parseInput = map parseLine . lines
    where parseLine ('R' : num) = read num
          parseLine ('L' : num) = - (read num)
          parseLine _ = error "Impossible case, check that your input is valid"

partOne :: Input -> Output
partOne = count ((== 0) . (`mod` 100)) . scanl (+) 50

partTwo :: Input -> Output
partTwo = sum . map fst . scanl step (0, 50)
    where  step (_, cur) rot = (abs turns + fromEnum clicks, (cur + left) `mod` 100)
            where (turns, left) = rot `quotRem` 100 -- Gives the number of complete turns and the remainder for the last turn
                  clicks = cur /= 0 && (left >= 100 - cur || left <= -cur) -- Gives whether or not the last turns passes or falls on 0


day01 :: String -> String -> IO ()
day01 "parse" = print . parseInput
day01 "one"   = print . partOne . parseInput
day01 "two"   = print . partTwo . parseInput
day01 "sone"  = submit 2025 1 1 . show . partOne . parseInput
day01 "stwo"  = submit 2025 1 2 . show . partTwo . parseInput
day01 _       = error "Undefined part"
