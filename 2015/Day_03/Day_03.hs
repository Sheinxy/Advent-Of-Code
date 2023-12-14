module Main where

import Data.List (nub)
import Data.Tuple.Extra (first, second)
import Data.Function ((&))
import System.Environment

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

compute :: Input -> String -> IO ()
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args 
