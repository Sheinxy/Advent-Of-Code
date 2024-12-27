module Main where

import           Data.Function
import           Data.Function.Memoize
import           Data.List
import           Data.List.Split
import           System.Environment

type Input = ([String], [String])
type Output = Int

parseInput :: String -> Input
parseInput input = (towels, designs)
    where p : _ : designs = lines input
          towels = sortBy (flip compare `on` length) $ splitOn ", " p

isPatternRec :: [String] -> (String  -> Bool) -> String -> Bool
isPatternRec towels f "" = True
isPatternRec towels f s  = any (f . flip drop s . length) $ filter (`isPrefixOf` s) towels

partOne :: Input -> Output
partOne (towels, designs) = length $ filter isPatternMem designs
    where isPatternMem = memoFix (isPatternRec towels)

numPatternRec :: [String] -> (String  -> Int) -> String -> Int
numPatternRec towels f "" = 1
numPatternRec towels f s  = sum . map (f . flip drop s . length) $ filter (`isPrefixOf` s) towels

partTwo :: Input -> Output
partTwo (towels, designs) = sum . map numPatternMem $ designs
    where numPatternMem = memoFix (numPatternRec towels)

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args
