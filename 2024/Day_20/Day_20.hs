module Main where

import System.Environment
import Data.Array.IArray
import Algorithm.Search
import Data.List
import Data.Maybe
import Control.Parallel.Strategies

data World = World { grid :: Array (Int, Int) Char, start :: (Int, Int), end :: (Int, Int) } deriving Show

type Input = World
type Output = Int

parseInput :: String -> Input
parseInput input = World grid start end
    where gridLines = lines input
          (height, width) = (length gridLines, length $ head gridLines)
          grid  = listArray ((1, 1), (height, width)) $ filter (/= '\n') input
          start = fst . head . filter ((== 'S') . snd) . assocs $ grid
          end   = fst . head . filter ((== 'E') . snd) . assocs $ grid

taxicab :: (Int, Int) -> (Int, Int) -> Int
taxicab (i, j) (k, l) = abs (i - k) + abs (j - l)

getRaceTrack :: Input -> [(Int, Int)]
getRaceTrack (World g s e) = fromJust $ bfs getNeighbours (== e) s
    where getNeighbours (i, j) = filter ((/= '#') . (g !)) [(i + di, j + dj) | di <- [-1 .. 1],
                                                                               dj <- [-1 .. 1],
                                                                               abs di + abs dj == 1]

getCheats :: [(Int, Int)] -> Int -> Int -> [((Int, Int), (Int, Int))]
getCheats path cheat_dist save = cheats
    where associations = zip path (drop (save + cheat_dist) path)
          cheats       = filter ((== cheat_dist) . uncurry taxicab) associations

partOne :: Input -> Output
partOne w = length cheats
    where path   = start w : getRaceTrack w
          len    = length path
          cheats = concat $ parMap rseq (getCheats path 2) [100, 102 .. len - 2]

partTwo :: Input -> Output
partTwo w = length cheats
    where path   = start w : getRaceTrack w
          len    = length path
          cheats = concatMap (getCheats' path) [2 .. 20]
          getCheats' path dist = concat $ parMap rseq (getCheats path dist) [100, 102 .. len - dist]

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args 
