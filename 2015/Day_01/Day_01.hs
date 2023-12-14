module Main where

import Data.Function ((&))
import System.Environment

type Input = [(Int -> Int)]
type Output = Int

parseInput :: String -> Input
parseInput = map getOp
    where getOp '(' = ( 1 +)
          getOp ')' = (-1 +)

partOne :: Input -> Output
partOne = foldr ($) 0

partTwo :: Input -> Output
partTwo = length . takeWhile (/= -1) . scanl (&) 0

compute :: Input -> String -> IO ()
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args 
