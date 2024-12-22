module Keyboard.Pathfinding  where

import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

numpad :: Map Char (Int, Int)
numpad = Map.fromList indexed
    where pad     = ["#0A", "123", "456", "789"]
          indexed = concat [zipWith (\c j -> (c, (i, j))) s [0 .. ] | (s, i) <- zip pad [0 .. ]]

keypad :: Map Char (Int, Int)
keypad = Map.fromList indexed
    where pad     = ["#^A", "<v>"]
          indexed = concat [zipWith (\c j -> (c, (i, j))) s [0 .. ] | (s, i) <- zip pad [0 .. ]]

computePaths :: Map Char (Int, Int) -> Char -> Char -> [String]
computePaths m src dst | i == 0 = filter (not . leadsToVoidH) paths
                       | j == 0 = filter (not . leadsToVoidV) paths
                       | otherwise = paths
    where (i, j) = m Map.! src
          (k, l) = m Map.! dst

          dx = l - j
          dy = k - i

          horDir | dx < 0    = '<'
                 | otherwise = '>'
          verDir | dy < 0    = 'v'
                 | otherwise = '^'

          moves = replicate (abs dy) verDir ++ replicate (abs dx) horDir
          paths = [moves, reverse moves] -- permutations moves 
          leadsToVoidH = ((replicate j '<') `isPrefixOf`)
          leadsToVoidV = ((replicate i 'v') `isPrefixOf`)

allNumpadPaths :: Map (Char, Char) [String]
allNumpadPaths = Map.fromList [((s, d), computePaths numpad s d) | s <- pad, d <- pad]
    where pad = filter (/= '#') $ Map.keys numpad

allKeypadPaths :: Map (Char, Char) [String]
allKeypadPaths = Map.fromList [((s, d), computePaths' s d) | s <- pad, d <- pad]
    where pad = filter (/= '#') $ Map.keys keypad
          computePaths' s d = map (map flipDir) $ computePaths keypad  s d
          flipDir '^' = 'v'
          flipDir 'v' = '^'
          flipDir c   = c

getNumpadPaths :: (Char, Char) -> [String]
getNumpadPaths = (allNumpadPaths Map.!)

getKeypadPaths :: (Char, Char) -> [String]
getKeypadPaths = (allKeypadPaths Map.!)
