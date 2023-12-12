module Main where

import Data.List (intercalate)
import Data.Maybe (isJust, fromJust)
import Data.Map hiding (map)
import qualified Data.Map as M (lookup)
import Data.Tuple.Extra ((***))
import System.Environment

type Input = [(String, [Int])]
type Output = Int

parseInput :: String -> Input
parseInput = map (go . words) . lines
    where go [s, ns] = (s, read $ "[" ++ ns ++ "]")

-- Memoization types
type Key = (Int, String, [Int]) -- (Current block length, tiles, groups)
type Memo = Map Key Int         -- State -> Number of arrangement given by that state

-- Update the cache and return both the cache and the value
updateMemo :: Memo -> Key -> Int -> (Memo, Int)
updateMemo m k v = (insert k v m, v)

-- Compute the number of possible arrangements with a cache
computeWithMemo :: Memo -> Key -> (Memo, Int)
computeWithMemo m k@(n, s, g) | isJust r = (m, fromJust r) where r = M.lookup k m         -- Cached result

computeWithMemo m k@(n, _, c:_)  | n > c = updateMemo m k 0                               -- Invalid arrangement: current block is too big

computeWithMemo m k@(0, xs, [ ]) | all (`elem` "?.") xs           = updateMemo m k 1      -- Valid arrangement: end of groups
computeWithMemo m k@(_, xs, [ ]) | '#'  `elem`       xs           = updateMemo m k 0      -- Invalid arrangement: more blocks but no more groups
computeWithMemo m k@(n, xs, [r]) | all (`elem` "?.") xs && r == n = updateMemo m k 1      -- Valid arrangement: current block ended with the right length for the last group
computeWithMemo m k@(n, "",   _) = updateMemo m k 0                                       -- Invalid arrangement: end of strings with leftover groups

computeWithMemo m k@(0, '.' : xs, groups) = updateMemo m' k v                                   -- Case '.' while not on block: keep going
                                          where (m', v) = computeWithMemo m (0, xs, groups)
computeWithMemo m k@(n, '.' : xs, c:rest) | n == c      = updateMemo m' k v                     -- Case '.' while on block: block has the right length
                                          | otherwise   = updateMemo m k 0                      -- Case '.' while on block: block has not the right length
                                          where (m', v) = computeWithMemo m (0, xs, rest)

computeWithMemo m k@(n, '#' : xs, groups) = updateMemo m' k v                                   -- Case '#': keep going and count this tile in the block length
                                          where (m', v) = computeWithMemo m (n + 1, xs, groups)

computeWithMemo m k@(n, '?' : xs, groups) = updateMemo m'' k (v + v')                           -- Case '?': Check what happens if '.' and if '#'. Add the number of arrangement for each possibility
                                          where (m' , v ) = computeWithMemo m  (n, '.' : xs, groups) 
                                                (m'', v') = computeWithMemo m' (n, '#' : xs, groups)
partOne :: Input -> Output
partOne = sum . map (\(s, g) -> snd . computeWithMemo empty $ (0, s, g))

partTwo :: Input -> Output
partTwo = partOne . map (intercalate "?" . replicate 5 *** concat . replicate 5)

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args 
