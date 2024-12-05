module Main where

import System.Environment
import Data.List

type Input = ([(Int, Int)], [[Int]])
type Output = Int

parseInput :: String -> Input
parseInput = getInput . break null . lines
    where getInput (orders, _ : numbers) = (map readOrder orders, map readNumber numbers)
          readOrder n = (read (takeWhile (/= '|') n), read (tail $ dropWhile (/= '|') n))
          readNumber n = read ("[" ++ n ++ "]") :: [Int]

getCorrectOrder :: [(Int, Int)] -> Int -> Int -> Ordering
getCorrectOrder orders a b
            | (a, b) `elem` orders = LT
            | (b, a) `elem` orders = GT
            | otherwise            = EQ

middle :: [a] -> a
middle l = l !! (length l `div` 2)

partOne :: Input -> Output
partOne (orders, input) = sum [middle a | (a, b) <- zip ordered input, a == b]
    where ordered = map (sortBy $ getCorrectOrder orders) input

partTwo :: Input -> Output
partTwo (orders, input) = sum [middle a | (a, b) <- zip ordered input, a /= b]
    where ordered = map (sortBy $ getCorrectOrder orders) input

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args 
