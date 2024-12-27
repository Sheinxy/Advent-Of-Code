module Main where

import           Data.Algorithm.MaximalCliques
import           Data.Function
import           Data.List
import           Data.List.Split
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           System.Environment

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

bronKerbosch :: Input -> [[String]]
bronKerbosch graph = bronKerbosch' Set.empty nodes Set.empty
    where nodes = Set.fromList $ Map.keys graph
          bronKerbosch' :: Set String -> Set String -> Set String -> [[String]]
          bronKerbosch' r p x | Set.null p && Set.null x = [Set.toList r]
                              | otherwise = res
            where (_, _, res) = Set.foldr go (x, p, []) p
                  go v (x, p, res) = (Set.insert v x, Set.delete v p, res ++ bronKerbosch' r' p' x')
                    where r' = Set.insert v r
                          p' = Set.intersection p $ Set.fromList (graph Map.! v)
                          x' = Set.intersection x $ Set.fromList (graph Map.! v)

partBonus :: Input -> String
partBonus = intercalate "," . sort .
            maximumBy (compare `on` length) .
            bronKerbosch

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = putStrLn . partTwo $ input
compute input "bonus" = putStrLn . partBonus $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args
