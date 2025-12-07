module Day07.Main (day07) where

import           AOC                   (submit)
import           AOC.Utils             (index2D)
import           Data.Function.Memoize
import           Data.List             (find)
import qualified Data.Set              as S
import           Data.Tuple.Extra      ((&&&))
import           Safe                  (findJust)

type Input = ((Int, Int), [(Int, Int)])
type Output = Int

parseInput :: String -> Input
parseInput = (fst . findJust ((== 'S') . snd) &&& map fst . filter ((== '^') . snd))
           . index2D . lines

findStop :: (Int, Int) -> [(Int, Int)] -> Maybe (Int, Int)
findStop (r, c) = find (\(i, j) -> r <= i && j == c)

partOne :: Input -> Output
partOne (start, splitters) = S.size . memoFix go $ start
    where go f s = case findStop s splitters of
                        Nothing     -> S.empty
                        Just (i, j) -> S.insert (i, j) $ S.union (f (i, j - 1)) (f (i, j + 1))

partTwo :: Input -> Output
partTwo (start, splitters) = memoFix go start
    where go f s = case findStop s splitters of
                        Nothing     -> 1
                        Just (i, j) -> f (i, j - 1) + f (i, j + 1)

day07 :: String -> String -> IO ()
day07 "parse" = print . parseInput
day07 "one"   = print . partOne . parseInput
day07 "two"   = print . partTwo . parseInput
day07 "sone"  = submit 2025 7 1 . show . partOne . parseInput
day07 "stwo"  = submit 2025 7 2 . show . partTwo . parseInput
day07 _       = error "Undefined part"
