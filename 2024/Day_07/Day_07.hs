module Main where

import System.Environment
import Data.List.Split

type Input = [(Int, [Int])]
type Output = Int

parseInput :: String -> Input
parseInput = map ((\[a, b] -> (read a, map read $ words b)) . splitOn ": ") . lines

partOne :: Input -> Output
partOne = sum . map fst . filter isDeducible
    where isDeducible (target, x : xs) = testOperations target x xs
          testOperations target acc [] = target == acc
          testOperations target acc (x : xs) = testOperations target (acc + x) xs ||
                                               testOperations target (acc * x) xs

partTwo :: Input -> Output
partTwo = sum . map fst . filter isDeducible
    where isDeducible (target, x : xs) = testOperations target x xs
          testOperations target acc [] = target == acc
          testOperations target acc (x : xs) = testOperations target (acc + x) xs ||
                                               testOperations target (acc * x) xs ||
                                               testOperations target (read $ show acc ++ show x) xs

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args 
