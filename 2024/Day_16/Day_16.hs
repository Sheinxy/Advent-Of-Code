module Main where

import System.Environment
import Algorithm.Search
import Data.Array.IArray
import Data.Maybe
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data World = World { grid :: Array (Int, Int) Char, start :: (Int, Int), end :: (Int, Int) } deriving Show

data State = State { position :: (Int, Int), direction :: (Int, Int) } deriving (Show, Eq, Ord)

type Input = World
type Output = Int

applyTuple :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
applyTuple f (a, b) (c, d) = (f a c, f b d)

getRotations :: (Int, Int) -> [(Int, Int)]
getRotations (di, dj) = [(di, dj), (dj, di), (-dj, -di)]

parseInput :: String -> Input
parseInput input = World grid start end
    where gridLines = lines input
          (height, width) = (length gridLines, length $ head gridLines)
          grid  = listArray ((1, 1), (height, width)) $ filter (/= '\n') input
          start = fst . head . filter ((== 'S') . snd) . assocs $ grid
          end   = fst . head . filter ((== 'E') . snd) . assocs $ grid

getNextStates :: Array (Int, Int) Char -> State -> [State]
getNextStates grid (State pos dir) = nexts
    where rotations = getRotations dir
          steps     = [State (applyTuple (+) pos d) d | d <- rotations]
          nexts     = filter ((/= '#') . (grid !) . position) steps

getCost :: State -> State -> Int
getCost s1 s2 | direction s1 /= direction s2 = 1001
              | otherwise                    = 1

partOne :: Input -> Output
partOne (World arr start end) = fst . fromJust $ dijkstra getNexts getCost isDone (State start (0, 1))
    where getNexts = getNextStates arr
          isDone   = (== end) . position


partTwo :: Input -> Output
partTwo input@(World arr start end) = length . nub . map position . Map.keys $ statesOnPath
    where target = partOne input
          states = dfs (State start (0, 1)) Map.empty 0
          statesOnPath = Map.filter fst states
          dfs x mem dist  | position x == end = Map.insert x (True, dist) mem
                          | dist >= bestDist  = mem
                          | otherwise         = mem''
            where (_, bestDist) = Map.findWithDefault (False, target) x mem
                  neighbours    = getNextStates arr x
                  costs         = map (getCost x) neighbours
                  tryVisit (next, cost) mem = dfs next mem (dist + cost)
                  isNeighbourOnPath cost (onPath, neighDist) = onPath && neighDist == dist + cost
                  mem' = foldr tryVisit mem (zip neighbours costs)
                  mem'' | or $ zipWith isNeighbourOnPath costs [Map.findWithDefault (False, target) n mem' | n <- neighbours] = Map.insert x (True, dist) mem'
                        | otherwise = Map.insert x (False, dist) mem'

partBonus :: Input -> (Output, Output)
partBonus input@(World arr start end) = (distOfEnd, tilesOnPath)
    where distance (i, j) (k, l) = abs (i - k) + abs (j - l)
          maxDist = 1001 * distance start end
          states = dfs (State start (0, 1)) Map.empty 0
          statesOnPath = Map.filter fst states
          distOfEnd    = snd . Map.findMin . Map.map snd $ Map.filterWithKey (\k _ -> position k == end) states
          tilesOnPath  = length . nub . map position . Map.keys $ statesOnPath
          dfs x mem dist  | position x == end = Map.insert x (True, dist) mem
                          | dist >= bestDist  = mem
                          | otherwise         = mem''
            where (_, bestDist) = Map.findWithDefault (False, maxDist) x mem
                  neighbours    = getNextStates arr x
                  costs         = map (getCost x) neighbours
                  tryVisit (next, cost) mem = dfs next mem (dist + cost)
                  isNeighbourOnPath cost (onPath, neighDist) = onPath && neighDist == dist + cost
                  mem' = foldr tryVisit mem (zip neighbours costs)
                  mem'' | or $ zipWith isNeighbourOnPath costs [Map.findWithDefault (False, maxDist) n mem' | n <- neighbours] = Map.insert x (True, dist) mem'
                        | otherwise = Map.insert x (False, dist) mem'

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
