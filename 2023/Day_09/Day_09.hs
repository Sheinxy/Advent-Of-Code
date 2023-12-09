module Main where

import System.Environment

type Input = [[Int]]
type Output = Int

parseInput :: String -> Input
parseInput = map (map read . words) . lines

generateSubsequence :: [Int] -> [[Int]]
generateSubsequence = takeWhile (not . all (== 0)) . iterate getDiffs
    where getDiffs l = zipWith (-) (tail l) l

partOne :: Input -> Output
partOne = sum . map (foldr ((+) . last) 0 . generateSubsequence)

partTwo :: Input -> Output
partTwo = sum . map (foldr ((-) . head) 0 . generateSubsequence)

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> (readFile $ last args)
    mapM (compute input)  $ init args 
