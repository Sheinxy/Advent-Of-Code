module Day04.Main (day04) where

import           AOC      (submit)
import           Data.Set ((\\))
import qualified Data.Set as S

type Input = S.Set (Int, Int)
type Output = Int

parseInput :: String -> Input
parseInput raw = S.fromList . map fst . filter ((== '@') . snd) $
            [ ((i, j), x)
            | (i, row) <- zip [0..] grid,
              (j, x) <- zip [0..] row
            ]
    where grid = lines raw

findAccessible :: Input -> Input
findAccessible rolls = S.filter isAccessible rolls
    where isAccessible (i, j) = length adjacents < 4
            where neighbours = [(i + di, j + dj) | di <- [-1 .. 1], dj <- [-1 .. 1], di /= 0 || dj /= 0]
                  adjacents = filter (`S.member` rolls) neighbours

partOne :: Input -> Output
partOne = S.size . findAccessible

partTwo :: Input -> Output
partTwo rolls = S.size rolls - S.size finalState
    where removeRolls rolls' = rolls' \\ findAccessible rolls'
          finalState = until (S.null . findAccessible) removeRolls rolls

day04 :: String -> String -> IO ()
day04 "parse" = print . parseInput
day04 "one"   = print . partOne . parseInput
day04 "two"   = print . partTwo . parseInput
day04 "sone"  = submit 2025 4 1 . show . partOne . parseInput
day04 "stwo"  = submit 2025 4 2 . show . partTwo . parseInput
day04 _       = error "Undefined part"
