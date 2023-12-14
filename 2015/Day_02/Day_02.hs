module Main where

import Data.Char
import Data.List
import Data.Function
import System.Environment

type Input = [[Int]]
type Output = Int

parseInput :: String -> Input
parseInput = map (map read . filter (all isDigit) . groupBy ((==) `on` isDigit)) . lines

partOne :: Input -> Output
partOne input = sum [ 2 * l * w + 2 * w * h + 2 * h * l + smallest l w h | [l, w, h] <- input]
    where smallest l w h = minimum [l * w, w * h, l * h]

partTwo :: Input -> Output
partTwo input = sum [ l * w * h + 2 * sum (take 2 $ sort x) | x@[l, w, h] <- input]

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args 
