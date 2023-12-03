module Main where

import Data.Char
import Data.List
import Data.Matrix (Matrix, fromLists, (!), nrows, ncols)
import System.Environment

data Number = Number { value :: Int, row :: Int, span :: (Int, Int) } deriving (Show)

-- Grid, Numbers, Potential Gears
type Input = (Matrix Char, [Number], [(Int, Int)])
type Output = Int

parseInput :: String -> Input
parseInput input = (grid, numbers, gears)
    where inLines = lines input
          grid    = fromLists inLines
          -- Getting the list of numbers
          numCol  = map (zip [1 .. ]) inLines
          grouped = map (groupBy (\a b -> isDigit (snd a) == isDigit (snd b))) numCol
          cleaned = map (map (\l -> ((fst $ head l, fst $ last l), map snd l))) grouped
          numRow  = zip [1 .. ] $ map (filter (\(_, n) -> all isDigit n)) cleaned
          numbers = concatMap (\(r, row) -> map (\(s, n) -> Number (read n) r s) row) numRow
          -- Getting the list of gears
          gearCols = map (filter ((== '*') . snd )) numCol
          gearRows = zip [1 .. ] gearCols
          gears = concatMap (\(r, row) -> map (\(c, _) -> (r, c)) row) gearRows

partOne :: Input -> Output
partOne (grid, numbers, _) = sum . map (value) . filter isAdjacent $ numbers
    where maxRow = nrows grid
          maxCol = ncols grid
          getNeighbours (Number _ r (c1, c2)) = map (grid !) [(i, j) | i <- [r - 1 .. r + 1], j <- [c1 - 1 .. c2 + 1], 0 < i && i <= maxRow, 0 < j && j <= maxCol]
          isAdjacent = any (\c -> not (isDigit c) && c /= '.') . getNeighbours

partTwo :: Input -> Output
partTwo (grid, numbers, gears) = sum . map (product . map value) . filter ((== 2) . length) . map getNeighbours $ gears
    where maxRow = nrows grid
          maxCol = ncols grid
          getNeighboursPos (r, c) = [(i, j) | i <- [r - 1 .. r + 1], j <- [c - 1 .. c + 1], 0 < i && i <= maxRow, 0 < j && j <= maxCol]
          getNeighbours g = filter (\(Number _ r (c1, c2)) -> any (`elem` neighbours) [(r, c) | c <- [c1 .. c2]]) numbers where neighbours = getNeighboursPos g

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> (readFile $ last args)
    mapM (compute input)  $ init args 
