module Main where

import           Data.List
import           Data.List.Split
import           System.Environment

type Input = [[Int]]
type Output = Int

parseInput :: String -> Input
parseInput = transpose . map (map read . splitOn "   ") . lines

partOne :: Input -> Output
partOne = sum . map abs . uncurry (zipWith (-)) . toPair . map sort
    where toPair [a, b] = (a, b)

partTwo :: Input -> Output
partTwo [a, b] = sum $ map (\x -> x * (x `countElem` b)) a
    where countElem a = length . filter (== a)

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args
