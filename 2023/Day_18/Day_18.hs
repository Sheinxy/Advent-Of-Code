module Main where

import Data.Char (digitToInt)
import Numeric (readHex)
import System.Environment

type Vertex = (Int ,Int)

type Input = [(String, Int, String)]
type Output = Int

parseInput :: String -> Input
parseInput = map (go . words) . lines
    where go [dir, dist, _:colour] = (dir, read dist, init colour)

digTranches :: Input -> [Vertex]
digTranches = scanl dig (0, 0)
    where dig (r, c) ("L", n, _) = (r    , c - n)
          dig (r, c) ("R", n, _) = (r    , c + n)
          dig (r, c) ("U", n, _) = (r - n, c    )
          dig (r, c) ("D", n, _) = (r + n, c    )

area :: [Vertex] -> Int
area vertices = 1 + perimeter `div` 2 + (abs . (`div` 2) . sum . zipWith crossProduct vertices $ tail vertices)
    where crossProduct (r1, c1) (r2, c2) = c1 * r2 - r1 * c2
          dist         (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)
          perimeter = sum . zipWith dist vertices $ tail vertices

convertColour :: (String, Int, String) -> (String, Int, String)
convertColour (_, _, '#':colour) = (newDir, newDist, "#ffffff")
    where distHex = init colour
          dirNum  = (digitToInt . last) colour
          newDir  = ["R", "D", "L", "U"] !! dirNum
          newDist = (fst . head . readHex) distHex

partOne :: Input -> Output
partOne = area . digTranches

partTwo :: Input -> Output
partTwo = partOne . map convertColour

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args 
