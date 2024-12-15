module Main where

import System.Environment
import Data.List
import Data.List.Split
import Data.Array.IArray
import Data.Tuple.Extra
import Data.Set (Set)
import qualified Data.Set as Set

data World = World { grid :: Array (Int, Int) Char, position :: (Int, Int) } deriving Show

type Input = (World, [Char])
type Output = Int

parseInput :: String -> Input
parseInput input = (World grid start, moves)
    where [gridStr, moveLines] = splitOn "\n\n" input
          moves                = filter (/= '\n') moveLines
          gridLines            = lines gridStr
          height               = length gridLines
          width                = length $ head gridLines
          grid                 = listArray ((0, 0), (height - 1, width - 1)) $ filter (/= '\n') gridStr
          start                = fst . head . filter ((== '@') . snd) . assocs $ grid

getStep :: Char -> (Int, Int)
getStep '<' = (0, -1)
getStep '>' = (0,  1)
getStep 'v' = (1,  0)
getStep '^' = (-1, 0)

applyTuple :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
applyTuple f (a, b) (c, d) = (f a c, f b d)

move :: World -> Char -> World
move world dir
        | any ((== '#') . (g !)) moves = world
        | otherwise                    = World g' pos'
    where step  = getStep dir
          g     = grid world
          pos   = position world
          pos'  = applyTuple (+) pos step
          moves = pos : propagateStep [pos'] (Set.singleton pos')

          g'    = g // [(p, getNew p ) | p <- moves]

          -- Get a tile's new state
          getNew p | prev `elem` moves = g ! prev -- If the previous tile was moved, then this tile's new state is the previous tile's 
                   | otherwise         = '.'      -- Otherwise, this tile is now empty
                   where prev = applyTuple (-) p step

          -- Propagates the movement along boxes
          propagateStep [] v = Set.toAscList v
          propagateStep (x : xs) v
            | current `elem` "#." = propagateStep xs v
            | otherwise           = propagateStep xs' v'
            where current = g ! x
                  neighbours | dir `elem` "v^" && current == '[' = nub [applyTuple (+) x (0, 1), applyTuple (+) x step]
                             | dir `elem` "v^" && current == ']' = nub [applyTuple (-) x (0, 1), applyTuple (+) x step]
                             | otherwise      = [applyTuple (+) x step]
                  neighbours' = filter (`Set.notMember` v) neighbours
                  xs'         = xs ++ neighbours'
                  v'          = foldr Set.insert v neighbours'

computeGPSScore :: Array (Int, Int) Char -> Output
computeGPSScore = sum . map (computeScore . fst) . filter ((`elem` "O[") . snd) . assocs
    where computeScore (i, j) = 100 * i + j

partOne :: Input -> Output
partOne = computeGPSScore . grid . uncurry (foldl move)

expandWorld :: World -> World
expandWorld world = World g' start
    where expandChar '#'  = "##"
          expandChar 'O'  = "[]"
          expandChar '.'  = ".."
          expandChar '@'  = "@."
          g               = grid world
          (height, width) = snd $ bounds g
          width'          = (width + 1) * 2
          g'              = listArray ((0, 0), (height, width' - 1)) . concatMap expandChar $ elems g
          start           = fst . head . filter ((== '@') . snd) . assocs $ g'

partTwo :: Input -> Output
partTwo = partOne . first expandWorld

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args 
