module Main where

import           Data.Function
import           Data.List
import           System.Environment

data World = World { antennas :: [[(Int, Int)]], height :: Int, width :: Int } deriving (Show)

type Input = World
type Output = Int

parseInput :: String -> Input
parseInput input =  World antennas height width
    where grid = lines input
          height = length grid
          width  = length $ head grid
          antennas = map (map fst)           $
                     groupBy ((==) `on` snd) $
                     sortOn snd              $
                     filter ((/= '.') . snd) $
                     zip
                     [(i, j) | i <- [1 .. height], j <- [1 .. width]] $
                     filter (/= '\n') input

getAntinodes :: [Int] -> (Int, Int) -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
getAntinodes steps (height, width) (i1, j1) (i2, j2) = antinodes
    where (di, dj)   = (i1 - i2, j1 - j2)
          an1        = takeWhile isInGrid [(i1 + t * di, j1 + t * dj) | t <- steps]
          an2        = takeWhile isInGrid [(i2 - t * di, j2 - t * dj) | t <- steps]
          antinodes  = an1 ++ an2
          isInGrid (i, j) = 1 <= i && i <= height && 1 <= j && j <= width

partOne :: Input -> Output
partOne world = length . nub . concat $
                [getAntinodes [1] dims a1 a2 | antennaGroup <- antennas world,
                                               a1 <- antennaGroup,
                                               a2 <- antennaGroup,
                                               a1 < a2]
    where dims = (height world, width world)

partTwo :: Input -> Output
partTwo world = length . nub . concat $
                [getAntinodes [0 .. ] dims a1 a2 | antennaGroup <- antennas world,
                                                   a1 <- antennaGroup,
                                                   a2 <- antennaGroup,
                                                   a1 < a2]
    where dims = (height world, width world)

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args
