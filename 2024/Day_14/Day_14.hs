module Main where

import System.Environment
import Text.Regex.TDFA
import Data.Tuple.Extra
import Data.List
import Data.Foldable

data Robot = Robot { position :: (Int, Int), velocity :: (Int, Int) } deriving (Show)

type Input = [Robot]
type Output = Int

size :: (Int, Int)
size = (101, 103)

parseInput :: String -> Input
parseInput = map parseRobot . lines
    where parseRobot :: String -> Robot
          parseRobot line = Robot pos vel
            where [_ : p : v : _] = line =~ "p=([0-9]+,[0-9]+) v=(-?[0-9]+,-?[0-9]+)" :: [[String]]
                  pos             = read ("(" ++ p ++ ")")
                  vel             = read ("(" ++ v ++ ")")

move :: Int -> Robot -> Robot
move seconds r = r'
    where (width, height) = size
          (x,   y) = position r
          (dx, dy) = both (* seconds) $ velocity r
          pos'     = ((x + dx) `mod` width, (y + dy) `mod` height)
          r'       = r { position = pos' }

isInQuadrant :: Robot -> Int
isInQuadrant (Robot (x, y) _)
    | x < midX && y < midY = 1
    | x > midX && y < midY = 2
    | x < midX && y > midY = 3
    | x > midX && y > midY = 4
    | otherwise            = 0
    where (midX, midY) = both (`div` 2) size

computeSafetyFactor :: Int -> Input -> Output
computeSafetyFactor seconds = product . 
                              map length .
                              group . sort . 
                              filter (/= 0) .
                              map (isInQuadrant . move seconds)

partOne :: Input -> Output
partOne = computeSafetyFactor 100

getGridAfter :: Int -> Input -> [String]
getGridAfter seconds rs = [ [ getRepresentingChar x y | x <- xs ] | y <- ys ]
    where (width, height) = size
          rs' = map (move seconds) rs
          xs   = [0 .. width  - 1]
          ys   = [0 .. height - 1]
          getRepresentingChar x y | any (\r -> position r == (x, y)) rs' = '#'
                                  | otherwise                            = ' '
          
partTwo :: Input -> IO ()
partTwo input =  for_ [0 .. fst size * snd size] $ \i -> do
                    print i
                    mapM putStrLn  . getGridAfter i $ input

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = partTwo input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args 
