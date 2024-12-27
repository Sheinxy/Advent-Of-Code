module Main where

import           Control.Parallel.Strategies
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Tuple.Extra
import           System.Environment

data Heading = North | South | West | East deriving (Show, Eq, Ord)

rotate :: Heading -> Heading
rotate North = East
rotate East  = South
rotate South = West
rotate West  = North

data World = World { obstacles :: Set (Int, Int),
                     position :: (Int, Int),
                     heading :: Heading,
                     height :: Int, width :: Int } deriving Show

type Input = World
type Output = Int

parseInput :: String -> Input
parseInput input = World obstacles start heading height width
    where grid   = lines input
          height = length grid
          width  = length . head $ grid
          indexedLines = concat $ zipWith
                         (\i s -> ([(i, j, c) | (j, c) <- zip [0 .. ] s])) [0 .. ]
                         grid
          (start, c)   = head [((i, j), c) | (i, j, c) <- indexedLines, c `notElem` ".#"]
          obstacles    = Set.fromList [(i, j) | (i, j, c) <- indexedLines, c == '#']
          heading | c == '^'  = North
                  | c == 'v'  = South
                  | c == '>'  = East
                  | otherwise = West

isOnEdge :: World -> (Int, Int) -> Bool
isOnEdge w (i, j) = i == 0 || i == height w - 1 || j == 0 || j == width w - 1

getGuardWalkLine :: World -> [(Int, Int)]
getGuardWalkLine w = takeWhile (`Set.notMember` obs) steps
    where obs     = obstacles w
          (i, j)  = position w
          steps | heading w == North = zip (reverse [0 .. i]) (repeat j)
                | heading w == South = zip [i .. height w - 1] (repeat j)
                | heading w == East  = zip (repeat i) [j .. width w - 1]
                | otherwise          = zip (repeat i) (reverse [0 .. j])

getGuardPath :: World -> Set (Int, Int)
getGuardPath world = snd3 $
                     until isDone go
                     (world, Set.empty, Set.empty)
    where isDone (w, _, p) = isOnEdge w (position w) || (position w, heading w) `Set.member` p
          go (w, v, pos) = (w { position = position', heading = heading'}, v', pos')
            where visited = getGuardWalkLine w
                  position' = last visited
                  heading'  = rotate $ heading w
                  v'        = foldr Set.insert v visited
                  pos'      = Set.insert (position w, heading w) pos

partOne :: Input -> Output
partOne = Set.size . getGuardPath

partTwo :: Input -> Output
partTwo input = length . filter id $ parMap rseq tryBlocking guardPath
    where guardPath   = Set.toList $ Set.delete (position input) (getGuardPath input)
          isLoop = not . any (isOnEdge input). Set.toList
          tryBlocking pos = isLoop (getGuardPath w)
            where w = input { obstacles = Set.insert pos (obstacles input) }


compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args
