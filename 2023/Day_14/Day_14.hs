{-# LANGUAGE NumericUnderscores #-}
module Main where

import Data.Map (Map, insert, empty, member, (!))
import Data.List (foldl', transpose, sort, intercalate)
import Data.List.Split (splitOn)
import System.Environment

data Direction = North | West | South | East deriving (Eq)
data Cycle     = Cycle { start :: Int, values :: [Int] } deriving (Show)

type Input = [String]
type Output = Int

parseInput :: String -> Input
parseInput = lines

rotate90 :: Input -> Input
rotate90 = map reverse . transpose

rotate180 :: Input -> Input
rotate180 = map reverse . reverse

rotateN90 :: Input -> Input
rotateN90 = rotate180 . rotate90

slide :: Direction -> Input -> Input
slide West  = rotate180 . slide East . rotate180
slide South = rotate90  . slide East . rotateN90
slide North = rotateN90 . slide East . rotate90 
slide East  = map slideRow
    where slideRow = intercalate "#" . map sort . splitOn "#"

getLoad :: Input -> Output
getLoad world = sum [i | (i, row) <- zip [1 .. ] (reverse world), char <- row, char == 'O']

partOne :: Input -> Output
partOne = getLoad . slide North

findCycle :: Input -> Cycle
findCycle world = go empty world 0
    where go seen world n | world `member` seen = Cycle (seen ! world) []
                          | otherwise           = Cycle start (getLoad world : nexts)
                          where world' = foldl' (flip slide) world [North, West, South, East]
                                (Cycle start nexts) = go (insert world n seen) world' (n + 1)
            
partTwo :: Input -> Output
partTwo world | 1_000_000_000 <= start cycle = values cycle !! 1_000_000_000 -- As if!
              | otherwise                    = values cycle !! idx
    where cycle    = findCycle world
          cycleLen = (length . values) cycle - start cycle
          idx      = (1_000_000_000 - start cycle) `rem` cycleLen + start cycle

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne   $ input
compute input "two"   = print . partTwo   $ input
compute input "cycle" = print . findCycle $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args 
