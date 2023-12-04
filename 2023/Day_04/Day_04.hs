module Main where

import Data.List (break, replicate)
import Data.Map (Map, adjust, toList, (!))
import qualified Data.Map as M (fromList)
import Data.Set (Set, fromList, intersection, size)
import System.Environment

data Card = Card { winning :: Set Int, numbers :: Set Int } deriving (Show)

type Input = [Card]
type Output = Int

parseInput :: String -> Input
parseInput = map (getNumbers . break (== '|') . drop (length "Card   1:")) . lines
    where getNumbers (w, _ : n) = Card (fromList . map read $ words w) (fromList . map read $ words n)

partOne :: Input -> Output
partOne = sum . map ((2 ^) . (+ (-1))) . filter (/= 0) . map (size . (\(Card w n) -> w `intersection` n))

partTwo :: Input -> Output
partTwo card = sum . map snd . toList . foldl go startingState $ zip [1 .. ] card
    where startingState = M.fromList . zip [1 .. ] $ replicate (length card) 1
          go m (id, Card w n) = foldl (flip $ adjust (+ (m ! id))) m [id + 1 .. id + size (w `intersection` n)]

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> (readFile $ last args)
    mapM (compute input)  $ init args 
