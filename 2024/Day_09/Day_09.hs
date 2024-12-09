module Main where

import System.Environment
import Data.Maybe
import Data.List
import Data.Char

type Input = [(Maybe Int, Int)]
type Output = Int

parseInput :: String -> Input
parseInput input = filter ((/= 0) . snd) $ zip blockTypes numbers
    where blockTypes = intersperse Nothing [Just i | i <- [0 .. ]]
          numbers    = map digitToInt . head . lines $ input

moveBlocks :: Input -> Input
moveBlocks input = go input endBlocks
    where endBlocks = reverse (filter (isJust . fst) input)
          go xs ((Just y, i) : _)
            | x == y = [(Just y, i)]
            | x >  y = []
            where Just x = fst . head $ dropWhile (isNothing . fst) xs
          go ((Nothing, i) : xs) ((Just y, j) : ys)
            | i == j    = (Just y, i) : go xs ys
            | i <  j    = (Just y, i) : go xs ((Just y, j - i) : ys)
            | otherwise = (Just y, j) : go ((Nothing, i - j) : xs) ys
          go (h@(Just _, _) : xs) ys = h : go xs ys

moveEntireBlocks :: Input -> Input
moveEntireBlocks = reverse . go  . reverse
    where go [] = []
          go (x@(Nothing, i) : xs) = x : go xs
          go (x@(Just  _, i) : xs)
            | any (canFitFileInBlock x) xs = (Nothing, i) : go xs'
            | otherwise                    = x : go xs
            where xs' = reverse $ placeFile x $ reverse xs
          canFitFileInBlock _ (Just _, _) = False
          canFitFileInBlock (_, i) (Nothing, j) = i <= j 
          placeFile file@(Just v, i) xs
            | i < j  = before ++ (file : (Nothing, j - i) : after)
            | i == j = before ++ (file : after)
            where (before, (_, j) : after) = break (canFitFileInBlock file) xs

computeChecksum :: Input -> Output
computeChecksum = fst .
                  foldl (\(acc, idx) (x, l) -> 
                          (acc + fromMaybe 0 x * sum [idx .. idx + l - 1],
                           idx + l))
                  (0, 0)

partOne :: Input -> Output
partOne = computeChecksum . moveBlocks

partTwo :: Input -> Output
partTwo = computeChecksum . moveEntireBlocks

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args 
