module Main where

import           Control.Parallel.Strategies
import           Data.List
import           Data.Matrix
import qualified Data.Map           as M
import qualified Data.Set           as S
import           System.Environment

data Input  = Input { start :: (Int, Int), end :: (Int, Int), grid :: Matrix Char } deriving (Show)
type Output = Int

type WGraph = M.Map (Int, Int) [(Int, (Int, Int))]

parseInput :: String -> Input
parseInput input = Input { start=start, end=end, grid=grid }
    where grid  = (fromLists . lines) input
          start = head [(1         , c) | c <- [1 .. ncols grid], grid ! (1         , c) == '.']
          end   = head [(nrows grid, c) | c <- [1 .. ncols grid], grid ! (nrows grid, c) == '.']

getNeighbours :: Bool -> (Int, Int) -> Matrix Char -> [(Int, Int)]
getNeighbours isSlippy pos@(r, c) grid = filter isNotWall . filter isInGrid $ neighbours
    where char             = grid ! pos
          isInGrid  (r, c) = 0 < r && r <= nrows grid && 0 < c && c <= ncols grid
          isNotWall (r, c) = grid ! (r, c) /= '#'
          neighbours | isSlippy && char == '>' = [(r, c + 1)]
                     | isSlippy && char == '<' = [(r, c - 1)]
                     | isSlippy && char == 'v' = [(r + 1, c)]
                     | isSlippy && char == '^' = [(r - 1, c)]
                     | otherwise               = [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

makeJunctionGraph :: Bool -> Input -> WGraph
makeJunctionGraph isSlippy (Input start end grid) = treatJunctions (S.singleton start) M.empty [start]
    where isJunction pos | pos == start || pos == end = True
                         | otherwise                  = length (getNeighbours isSlippy pos grid) > 2
          treatJunctions _ graph [] = graph
          treatJunctions seen graph (x:queue) = treatJunctions seen' graph' queue'
            where nextJunctions = findNextJunctions (S.singleton x) [(0, x)]
                  graph'    = M.insert x nextJunctions graph
                  junctions = filter (`S.notMember` seen) $ map snd nextJunctions
                  seen'     = foldr S.insert seen junctions
                  queue'    = queue ++ junctions
          findNextJunctions _ [] = []
          findNextJunctions seen ((d, x):queue) | d > 0 && isJunction x = (d, x) : findNextJunctions seen queue
                                                | otherwise             =          findNextJunctions seen' queue'
            where neighbours = filter (`S.notMember` seen) $ getNeighbours isSlippy x grid
                  seen'      = foldr S.insert seen neighbours
                  queue'     = queue ++ zip (repeat (d + 1)) neighbours

findLongestPath :: Bool -> Input -> Output
findLongestPath isSlippy input = go S.empty 0 (start input)
    where graph = makeJunctionGraph isSlippy input
          go seen pathLen cur | cur == end input = pathLen
                              | otherwise        = best
                              where seen'        = S.insert cur seen
                                    neighbours   = [(dist, pos) | (dist, pos) <- graph M.! cur, pos `S.notMember` seen]
                                    best         = maximum (0 : parMap rseq (uncurry (go seen' . (pathLen +))) neighbours)

partOne :: Input -> Output
partOne = findLongestPath True

partTwo :: Input -> Output
partTwo = findLongestPath False

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args 
