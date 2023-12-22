module Main where

import Control.Parallel.Strategies
import Data.Function
import Data.Ord
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tuple.Extra (both, thd3, third3)
import System.Environment

data Brick = Brick { minZ :: Int, maxZ :: Int, cubes :: S.Set (Int, Int, Int), supporting :: S.Set Int} deriving (Eq, Ord)

-- I am just having fun with instances here. I had no actual reason to make a show and read instance, but I wanted to
instance Read Brick where
    readsPrec _ s = [(Brick minZ maxZ cubes S.empty, "")]
        where (edge1, '~':edge2)           = break (=='~') . head . words $ s
              ((x1, y1, z1), (x2, y2, z2)) = both (read . ('(' :) . (++ ")")) (edge1, edge2)
              minZ  = min z1 z2
              maxZ  = max z1 z2
              cubes = S.fromList [(x, y, z) | x <- [min x1 x2 .. max x1 x2]
                                            , y <- [min y1 y2 .. max y1 y2]
                                            , z <- [min z1 z2 .. max z1 z2]]

-- This one is just for fun (and easier debugging in case there are like 1000 cubes in a brick)
instance Show Brick where
    show (Brick _ _ c s) = firstCube ++ "~" ++ lastCube ++
                            " | Number of cubes = " ++ show (S.size c) ++
                            " | Supported by = "    ++ show s
        where firstCube = init . tail . show $ S.findMin c
              lastCube  = init . tail . show $ S.findMax c

type Input = M.Map Int Brick
type Output = Int

fallDown :: [Brick] -> M.Map Int Brick
fallDown = foldl' go M.empty
    where ground = S.singleton (-1)  -- Singleton for bricks supported only by the ground
          go fall cur | z == 0        = M.insert i (cur { minZ=z+1 , maxZ=mz+1 , cubes=backCubes, supporting=ground }) fall -- Touched the ground
                      | null touching = go fall    (cur { minZ=z+dz, maxZ=mz+dz, cubes=nextCubes }) -- Touched no bricks
                      | otherwise     = M.insert i (cur { minZ=z+1 , maxZ=mz+1 , cubes=backCubes, supporting=touching }) fall
                      where i         = M.size fall -- The index for the current brick
                            z         = minZ cur    -- The bottom z value of the brick
                            mz        = maxZ cur    -- The top    z value of the brick
                            -- Speeding up the process by going more that one step at a time
                            nextZ     = maximum $ 0 : [maxZ c | c <- M.elems fall, maxZ c < z]
                            dz        = nextZ - z
                            -- Find the supporting bricks at that z step
                            touching  = M.keysSet . M.filter (not . S.disjoint (cubes cur) . cubes) $ fall
                            -- Get the cubes for going up or continuing falling done
                            backCubes = S.map (third3 succ ) . cubes $ cur
                            nextCubes = S.map (third3 (+dz)) . cubes $ cur

parseInput :: String -> Input
parseInput = fallDown . sort . map read . lines

getUnremovableBricks :: Input -> S.Set Int
getUnremovableBricks = S.filter (-1 /=) . M.foldr go S.empty
   where go cur acc | (S.size . supporting) cur == 1 = acc `S.union` supporting cur
                    | otherwise                      = acc

partOne :: Input -> Output
partOne input = M.size input - (S.size . getUnremovableBricks) input

-- Get the bricks that are no longer being supported
getFalling :: Input -> S.Set Int
getFalling = M.keysSet . M.filter (S.null . supporting)

-- Remove a set of bricks from the bricks
removeBricks :: S.Set Int -> Input -> Input
removeBricks toRemove = M.map removeSupporting . (`M.withoutKeys` toRemove)
    where removeSupporting c = c { supporting=S.difference (supporting c) toRemove }

-- Remove a brick and let the chain reaction happen
disintegrate :: Input -> Int -> Input
disintegrate bricks removed = until (null . getFalling) removeFalling startState -- Fix-point iteration
    where toRemove          = S.singleton removed
          startState        = removeBricks toRemove bricks
          removeFalling     = removeBricks =<< getFalling

partTwo :: Input -> Output
partTwo input = sum fallenCount
    where toRemove        = (S.toList . getUnremovableBricks) input
          disintegrations = parMap rseq (disintegrate input) toRemove
          totalBricks     = M.size input
          fallenCount     = map (subtract 1 . (totalBricks -) . M.size) disintegrations

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args 
