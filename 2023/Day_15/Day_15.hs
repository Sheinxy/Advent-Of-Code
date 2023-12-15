module Main where

import Data.Char
import Data.List.Split
import Data.Map (Map, (!), adjust, fromList, toList)
import System.Environment

data Lens = Lens { label :: String, focal :: Int } deriving (Show)
type Boxes = Map Int [Lens]

type Input = [String]
type Output = Int

parseInput :: String -> Input
parseInput = splitOn "," . head . lines

getHash :: String -> Int
getHash = foldl (\acc x -> (acc + ord x) * 17 `rem` 256) 0

putInBoxes :: Boxes -> String -> Boxes
putInBoxes boxes = go . span (`notElem` "=-")
    where go (lab, "-"    ) = adjust (filter ((/= lab) . label)) (getHash lab) boxes
          go (lab, '=' : n) | lab `elem` labels     = adjust (const $ before ++ [Lens lab (read n)] ++ after) hash boxes
                            | otherwise             = adjust (Lens lab (read n) :) hash boxes
                            where hash              = getHash lab
                                  elements          = boxes ! getHash lab
                                  labels            = map label elements
                                  (before, _:after) = span ((/= lab) . label) elements

partOne :: Input -> Output
partOne = sum . map getHash

partTwo :: Input -> Output
partTwo = sum . map getPower . toList . foldl putInBoxes (fromList [(i, []) | i <- [0 .. 255]])
    where getPower (i, xs) = sum [(i + 1) * j * focal lens | (j, lens) <- zip [1 .. ] $ reverse xs]

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args 
