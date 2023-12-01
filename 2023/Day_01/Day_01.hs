module Main where

import Data.Char
import Data.List
import System.Environment

type Input = [String]
type Output = Int

parseInput :: String -> Input
parseInput = lines

partOne :: Input -> Output
partOne = sum . map (\s -> read $ [head s, last s] ) . map (filter isDigit)

partTwo :: Input -> Output
partTwo = partOne . map go
    where go "" = ""
          go line@(c : xs)
            | "one"   `isPrefixOf` line = '1' : go xs
            | "two"   `isPrefixOf` line = '2' : go xs
            | "three" `isPrefixOf` line = '3' : go xs
            | "four"  `isPrefixOf` line = '4' : go xs
            | "five"  `isPrefixOf` line = '5' : go xs
            | "six"   `isPrefixOf` line = '6' : go xs
            | "seven" `isPrefixOf` line = '7' : go xs
            | "eight" `isPrefixOf` line = '8' : go xs
            | "nine"  `isPrefixOf` line = '9' : go xs
            | otherwise                 =  c  : go xs

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> (readFile $ last args)
    mapM (compute input)  $ init args
