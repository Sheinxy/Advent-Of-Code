module Main where

import Control.Parallel.Strategies
import Data.Matrix
import Data.Set (Set, size, notMember, empty, insert, singleton)
import qualified Data.Set as S (map)
import System.Environment

data Direction = North | South | East | West deriving (Show, Eq, Ord)

data Move = Move { position :: (Int, Int), direction :: Direction } deriving (Show, Eq, Ord)

type Input = Matrix Char
type Output = Int

parseInput :: String -> Input
parseInput = fromLists . lines

-- Will clean that up later maybe
getNexts :: Move -> Input -> [Move]
getNexts (Move (r, c) North) grid | char `elem` ".|" = [Move (r - 1, c    ) North]
                                  | char == '/'      = [Move (r    , c + 1) East ]
                                  | char == '\\'     = [Move (r    , c - 1) West ]
                                  | char == '-'      = [Move (r    , c - 1) West, Move (r, c + 1) East]
                                  where char = grid ! (r, c)

getNexts (Move (r, c) South) grid | char `elem` ".|" = [Move (r + 1, c    ) South ]
                                  | char == '\\'     = [Move (r    , c + 1) East  ]
                                  | char == '/'      = [Move (r    , c - 1) West  ]
                                  | char == '-'      = [Move (r    , c - 1) West, Move (r , c + 1) East]
                                  where char = grid ! (r, c)

getNexts (Move (r, c) East) grid  | char `elem` ".-" = [Move (r    , c + 1) East  ]
                                  | char == '\\'     = [Move (r + 1, c    ) South ]
                                  | char == '/'      = [Move (r - 1, c    ) North ]
                                  | char == '|'      = [Move (r - 1, c    ) North, Move (r + 1, c) South]
                                  where char = grid ! (r, c)

getNexts (Move (r, c) West) grid  | char `elem` ".-" = [Move (r    , c - 1) West  ]
                                  | char == '\\'     = [Move (r - 1, c    ) North ]
                                  | char == '/'      = [Move (r + 1, c    ) South ]
                                  | char == '|'      = [Move (r - 1, c    ) North, Move (r + 1, c) South]
                                  where char = grid ! (r, c)

bfs :: Set Move -> [Move] -> Input -> Int
bfs seen []     _    = size . S.map position $ seen
bfs seen (x:xs) grid = bfs seen' queue grid
    where nexts   = getNexts x grid
          inGrid  = filter (\(Move (r, c) _) -> 0 < r  && r <= nrows grid && 0 < c && c <= ncols grid) nexts
          notSeen = filter (`notMember` seen) inGrid
          seen'   = foldr insert seen notSeen
          queue   = xs ++ notSeen

partOne :: Input -> Output
partOne = bfs (singleton (Move (1, 1) East)) [Move (1, 1) East]

partTwo :: Input -> Output
partTwo grid = maximum possibilities
    where nr     = nrows grid
          nc     = ncols grid
          starts = [Move (1 , col) South | col <- [1 .. nc]] ++ [Move (row, 1 ) East | row <- [1 .. nr]] ++
                   [Move (nr, col) North | col <- [1 .. nc]] ++ [Move (row, nc) West | row <- [1 .. nr]]
          launch mv     = bfs (singleton mv) [mv] grid
          possibilities = parMap rseq launch starts

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args 
