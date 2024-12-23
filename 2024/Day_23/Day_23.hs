module Main where

import System.Environment
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Algorithm.MaximalCliques
import Data.Function

type Input = Map String [String]
type Output = Int

parseInput :: String -> Input
parseInput input = graph
    where alterVertex a Nothing   = Just [a]
          alterVertex a (Just xs) = Just (a : xs)
          addMap [a, b] m = Map.alter (alterVertex a) b $ Map.alter (alterVertex b) a m
          edges = map (splitOn "-") . lines $ input
          graph = foldr addMap Map.empty edges

findConnected :: Input -> String -> [[String]]
findConnected graph t = [sort [t, a, b] | a <- neighbours,
                                          b <- neighbours,
                                          a < b,
                                          b `elem` (graph Map.! a)]
    where neighbours = graph Map.! t

partOne :: Input -> Output
partOne input = length . nub $ concatMap (findConnected input) ts
    where ts = filter (('t' ==) . head) $ Map.keys input

partTwo :: Input -> String
partTwo input = intercalate "," . sort $ maxClique
    where nodes = Map.keys input
          isAdjacent a = (`elem` (input Map.! a))
          maxClique = maximumBy (compare `on` length) $ getMaximalCliques isAdjacent nodes

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = putStrLn . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args 
