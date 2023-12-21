module Main where

import Data.Either
import Data.List (stripPrefix)
import Data.Matrix
import Data.NumInstances.Tuple
import Data.Set (Set, notMember, insert, singleton, size, unions, empty)
import qualified Data.Set as S (filter, map, fromList)
import System.Environment

data Input = Input { start :: (Int, Int), grid :: Matrix Char } deriving (Show)
type Output = Int

parseInput :: String -> Input
parseInput input = Input start grid
    where grid  = (fromLists . lines) input
          start = head [(i, j) | (i,  row) <- zip [1 .. ] (lines input), 
                                 (j, char) <- zip [1 .. ] row,
                                 char == 'S']

getReachable :: Int -> Input -> [Int]
getReachable n (Input start grid) = drop 2 . reverse . bfs (0, singleton start) empty $ [0, 0]
    where wrapAround (r, c) = ((r - 1) `mod` nrows grid + 1, (c - 1) `mod` ncols grid + 1)
          isInWall   pos    = '#' == grid ! pos
          bfs (step, poss) old acc | step == n = acc'
                                   | otherwise = bfs state' poss acc'
              where neighboursOf pos = S.fromList                           .
                                       filter (not . isInWall . wrapAround) .
                                       filter (`notMember` old)             $
                                       [pos + dp | dp <- [(-1, 0), (1, 0), (0, -1), (0, 1)]]
                    acc'             = (acc !! 1 + size poss) : acc
                    state'           = (step + 1, unions . S.map neighboursOf $ poss)

partOne :: Input -> Output
partOne = last . getReachable 64

partTwo :: Input -> Output
partTwo input = result
    where rows     = nrows $ grid input
          -- Get the results at three points: (half height, half height + height, half height + 2 height)
          -- We will call them x1, x2, x3
          results  = getReachable ((5 * rows) `div` 2) input
          xs       = [i * rows + rows `div` 2 | i <- [0 .. 2]]
          ys       = [fromIntegral $ results !! x | x <- xs]

          {- Make two matrices: 
                - a 3x3 matrix where each row is [x ^ 2, x, 1] for each x in our three points
                - a 3x1 matrix where each row is the number of tiles reachable after x1, x2 and x3
          -}
          matX     = fromLists [map fromIntegral [x ^ 2, x, 1] | x <- xs]
          matY     = fromList 3 1 ys

          -- Solve matY = matX * coefs for unknown coefs
          Right iX = inverse matX
          coefs    = iX `multStd` matY

          -- Get the [x^2, x, 1] for the target step count
          target   = 26501365
          targetX  = fromList 1 3 [target ^ 2, target, 1]
          
          -- Do targetX * coefs and get the result
          result   = round $ (targetX `multStd` coefs) ! (1, 1)

plot :: (Int, Int) -> Input -> String
plot (width, height) input = unlines rows'
    where results     = getReachable width input
          pointHeight = (last results `div` 100 + 1) * 100 `div` (height - 1)
          getChar row res | res `div` pointHeight == row = 'X'
                          | otherwise                    = '.'
          rows        =  [[getChar row res | res <- results] | row <- [0 .. height]]
          headerRow   = replicate (length results) '-'
          rows'       = map ('|' :) . reverse $ headerRow : rows

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input "plot"  = putStrLn . plot (131, 20) $ input
compute input arg     | Just params <- stripPrefix "plot=" arg = putStrLn . plot (read params) $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args 
