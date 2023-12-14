-- Initial version that I used to solve the puzzle.
-- It is very slow (~25s on my computer to run) because the slide function is more or less O(n^2)
-- The reason I first did it like this is actually because I didn't want to have to handle the whole grid (manipulating grids in Haskell scares me :)). I ended up shooting myself in the foot and I prefered reworking my code before pushing it.

{-# LANGUAGE NumericUnderscores #-}
module Main where

import Data.Map (Map, insert, empty, member, (!))
import Data.List (sort, foldl')
import System.Environment

data Direction = North | West | South | East deriving (Eq)
data Cycle     = Cycle { start :: Int, values :: [Int] } deriving (Show)

data Input  = Input { nrows :: Int, ncols :: Int, rounds :: [(Int, Int)], cubes :: [(Int, Int)]} deriving (Eq, Ord, Show)
type Output = Int

parseInput :: String -> Input
parseInput input = Input nrows ncols rounds cube
    where rows   = lines input
          nrows  = length rows
          ncols  = length $ head rows
          rounds = [(i, j) | (i, row) <- zip [0 .. ] rows, (j, char) <- zip [0 .. ] row, char == 'O']
          cube   = [(i, j) | (i, row) <- zip [0 .. ] rows, (j, char) <- zip [0 .. ] row, char == '#']

rotate90 :: Input -> Input
rotate90 (Input nrows ncols rounds cubes) = Input ncols nrows rounds' cubes'
    where rotatePoint (row, col) = (col, nrows - 1 - row)
          rounds'                = map rotatePoint rounds
          cubes'                 = map rotatePoint cubes

rotate180 :: Input -> Input
rotate180 = rotate90 . rotate90

rotateN90 :: Input -> Input
rotateN90 = rotate180 . rotate90

slide :: Direction -> Input -> Input
slide East  world = rotate90  . slide North . rotateN90 $ world
slide South world = rotate180 . slide North . rotate180 $ world
slide West  world = rotateN90 . slide North . rotate90  $ world
slide North world = world { rounds = rounds' }
    where rounds' = foldl' slideBoulder [] . sort $ rounds world
          cs      = cubes world
          slideBoulder rs (row, col) = (row', col) : rs
            where row' = 1 + maximum ([-1] ++ [br | (br, bc) <- rs, br < row, bc == col] ++ 
                                                [cr | (cr, cc) <- cs, cr < row, cc == col])

getLoad :: Input -> Output
getLoad world = sum . map ((nrows world -) . fst) . rounds $ world

partOne :: Input -> Output
partOne = getLoad . slide North

findCycle :: Input -> Cycle
findCycle world = go empty world 0
    where go seen world n | world `member` seen = Cycle (seen ! world) []
                          | otherwise           = Cycle start (getLoad world : nexts)
                          where world' = foldl' (flip slide) world [North, West, South, East]
                                (Cycle start nexts) = go (insert world n seen) world' (n + 1)
            

partTwo :: Input -> Output
partTwo world | 1_000_000_000 <= start cycle = values cycle !! 1_000_000_000 -- As if!
              | otherwise                    = values cycle !! idx
    where cycle    = findCycle world
          cycleLen = (length . values) cycle - start cycle
          idx      = (1_000_000_000 - start cycle) `rem` cycleLen + start cycle

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args 
