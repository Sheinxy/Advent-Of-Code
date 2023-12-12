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
type Key = (String, Int, [Int]) -- (Tiles, Current block length, Groups left)
type Memo = Map Key Int         -- State -> Number of arrangement given by that state

-- Update the cache and return both the cache and the value
updateMemo :: Memo -> Key -> Int -> (Memo, Int)
updateMemo m k v = (insert k v m, v)

-- Compute the number of possible arrangements with a cache
computeWithMemo :: Memo -> Key -> (Memo, Int)
computeWithMemo m k | isJust r = (m, fromJust r) where r = M.lookup k m                         -- Cached result

computeWithMemo m k@(_, n, c:_) | n > c = updateMemo m k 0                                      -- Invalid arrangement: current block is too big

computeWithMemo m k@(xs      , 0, [ ]) | all (`elem` "?.") xs           = updateMemo m k 1      -- Valid arrangement: end of groups
computeWithMemo m k@(xs      , _, [ ]) | '#'  `elem`       xs           = updateMemo m k 0      -- Invalid arrangement: more blocks but no more groups
computeWithMemo m k@(xs      , n, [r]) | all (`elem` "?.") xs && r == n = updateMemo m k 1      -- Valid arrangement: current block ended with the right length for the last group
computeWithMemo m k@(""      , _,  _ ) = updateMemo m k 0                                       -- Invalid arrangement: end of strings with leftover groups

computeWithMemo m k@('.' : xs, 0, groups) = updateMemo m' k v                                   -- Case '.' while not on block: keep going
                                          where (m', v) = computeWithMemo m (xs, 0, groups)
computeWithMemo m k@('.' : xs, n, c:rest) | n == c      = updateMemo m' k v                     -- Case '.' while on block: block has the right length
                                          | otherwise   = updateMemo m k 0                      -- Case '.' while on block: block has not the right length
                                          where (m', v) = computeWithMemo m (xs, 0, rest)

computeWithMemo m k@('#' : xs, n, groups) = updateMemo m' k v                                   -- Case '#': keep going and count this tile in the block length
                                          where (m', v) = computeWithMemo m (xs, n + 1, groups)

computeWithMemo m k@('?' : xs, n, groups) = updateMemo m'' k (v + v')                           -- Case '?': Check what happens if '.' and if '#'. Add the number of arrangement for each possibility
                                          where (m' , v ) = computeWithMemo m  ('.' : xs, n, groups)
                                                (m'', v') = computeWithMemo m' ('#' : xs, n, groups)
partOne :: Input -> Output
partOne = sum . map (\(s, g) -> snd . computeWithMemo empty $ (s, 0, g))

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
