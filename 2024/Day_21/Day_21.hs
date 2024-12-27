module Main where

import           Data.Function
import           Data.Function.Memoize
import           Data.List
import           System.Environment

import           Keyboard.Pathfinding

type Input = [String]
type Output = Int

parseInput :: String -> Input
parseInput = lines

findPresses :: ((Char, Char) -> [String]) -> String -> [[String]]
findPresses getPaths s = map (map (++ "A")) $ presses
    where s'      = 'A' : s
          moves   = zip s' s
          presses = map getPaths moves

findNumPresses :: String -> [[String]]
findNumPresses = findPresses getNumpadPaths

findDirPresses :: String -> [[String]]
findDirPresses = findPresses getKeypadPaths

findSequence :: (Int -> String -> Int) -> Int -> String -> Int
findSequence f 0 s = length s
findSequence f n s = getRes s
    where findSequence' = f (n - 1)
          getRes  = minimum .
                    map (sum . map findSequence') .
                    sequence .
                    findDirPresses

solveWith :: Int -> String -> Int
solveWith robots = minimum .
                   map (sum . map (findSequenceMem robots)) .
                   sequence .
                   findNumPresses
    where findSequenceMem = memoFix2 findSequence

partOne :: Input -> Output
partOne = sum . map computeComplexity
    where computeComplexity code = (read . init) code * solveWith 2 code

partTwo :: Input -> Output
partTwo = sum . map computeComplexity
    where computeComplexity code = (read . init) code * solveWith 25 code

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args
