module Main where

import System.Environment
import Algorithm.Search
import Data.Maybe
import Data.Set (fromList, notMember)

type Input = [(Int, Int)]
type Output = Int

numCorrupted :: Int
numCorrupted = 1024

size :: Int
size = 70

parseInput :: String -> Input
parseInput = map parseLine . lines
    where parseLine line = read $ "(" ++ line ++ ")"

findPath :: Int -> Input -> Maybe [(Int, Int)]
findPath numWalls input = bfs getNextState isDone (0, 0)
    where walls = fromList $ take numWalls input
          getNextState (i, j) = filter (`notMember` walls) [(i + di, j + dj) | di <- [-1 .. 1],
                                                                               dj <- [-1 .. 1],
                                                                               abs di + abs dj == 1,
                                                                               0 <= i + di && i + di <= size,
                                                                               0 <= j + dj && j + dj <= size]
          isDone = (== (size, size))

partOne :: Input -> Output
partOne = length . fromJust . findPath numCorrupted

partTwo :: Input -> (Int, Int)
partTwo input = input !! notBlocking
    where notBlocking = numCorrupted + (length . takeWhile isJust . map (`findPath` input) $ [numCorrupted + 1.. ])

partBonus :: Input -> (Int, Int)
partBonus input = input !! notBlocking
    where numWalls = length input
          notBlocking = dichotomic numCorrupted numWalls - 1
          dichotomic low high | low + 1 == high = high
                              | isJust $ findPath m input  = dichotomic m high
                              | otherwise                  = dichotomic low m
            where m = (low + high) `div` 2

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input "bonus" = print . partBonus $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args 
