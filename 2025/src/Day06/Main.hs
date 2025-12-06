module Day06.Main (day06) where

import           AOC              (submit)
import           Data.List        (transpose, uncons)
import           Data.List.Split  (splitWhen)
import           Data.Maybe       (fromJust)
import           Data.Tuple.Extra ((***))

type Input = [(Int -> Int -> Int, [Int])]
type Output = Int

parseOperator :: String -> (Int -> Int -> Int)
parseOperator "*" = (*)
parseOperator "+" = (+)
parseOperator _   = error "Something went wrong, please check your input."

partOne :: String -> Input
partOne = map ((parseOperator *** map read) . fromJust . uncons . reverse)
        . transpose . map words . lines

partTwo :: String -> Input
partTwo = map parseProblem . splitWhen (all (== ' ')) . transpose . lines
    where parseProblem [] = error "Something went wrong, please check your input."
          parseProblem (x : xs) = (parseOperator [last x], map read $ init x : xs)

compute :: Input -> Output
compute = sum . map (uncurry foldl1)

day06 :: String -> String -> IO ()
day06 "one"  = print . compute . partOne
day06 "two"  = print . compute . partTwo
day06 "sone" = submit 2025 6 1 . show . compute . partOne
day06 "stwo" = submit 2025 6 2 . show . compute . partTwo
day06 _      = error "Undefined part"
