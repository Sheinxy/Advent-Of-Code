## Day 03

It is already time for 2D array manipulation in Haskell! 😿

```hs
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
```

There is a lot to unpack here so let's start by the input:
```hs
-- Grid, Numbers, Potential Gears
type Input = (Matrix Char, [Number], [(Int, Int)])
```
There are three interesting parts here: the actual grid (basically the input, almost raw, just wrapped inside a Data.Matrix),
the list of numbers and the list of potential grids represented by their coordinates.

Numbers are represented with a data structure containing their value, their row, and their span (first column and last column)

Parsing the input is hellish:
```hs
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
```

There are three main parts:
```hs
    where inLines = lines input
          grid    = fromLists inLines
```
This is the easy part: get the lines from the input, and transform it into a matrix

Now for the hard part:
```hs
          -- Getting the list of numbers
          numCol  = map (zip [1 .. ]) inLines
          grouped = map (groupBy (\a b -> isDigit (snd a) == isDigit (snd b))) numCol
          cleaned = map (map (\l -> ((fst $ head l, fst $ last l), map snd l))) grouped
          numRow  = zip [1 .. ] $ map (filter (\(_, n) -> all isDigit n)) cleaned
          numbers = concatMap (\(r, row) -> map (\(s, n) -> Number (read n) r s) row) numRow
```
I start by labelling each column in each row, then I group every number digit together (so I get the numbers whole in the form [(col1, digit 1), (col2, digit2) ...]).

After that, I clean things around by taking only the first and last column for each digit, and putting the digits together in a list: ((col1, colLast), [digit1, digit2, ...])

Now that I have that, I only keep the actual numbers by filtering on the digits to make sure they're ACTUAL digits, and I now label the rows.

Finally, for each row I transform every (span, digits) into a number data structure, and I flatten my grid into a single list.


Now the second hardest part:
```hs
          -- Getting the list of gears
          gearCols = map (filter ((== '*') . snd )) numCol
          gearRows = zip [1 .. ] gearCols
          gears = concatMap (\(r, row) -> map (\(c, _) -> (r, c)) row) gearRows
```
First I keep the * from the labeled column list, next I label the rows, and finally I transform these into coordinates.


This was the hard part (the list of gears was done after I did part one btw)

Now for the puzzles themselves:

```hs
partOne :: Input -> Output
partOne (grid, numbers, _) = sum . map (value) . filter isAdjacent $ numbers
    where maxRow = nrows grid
          maxCol = ncols grid
          getNeighbours (Number _ r (c1, c2)) = map (grid !) [(i, j) | i <- [r - 1 .. r + 1], j <- [c1 - 1 .. c2 + 1], 0 < i && i <= maxRow, 0 < j && j <= maxCol]
          isAdjacent = any (\c -> not (isDigit c) && c /= '.') . getNeighbours
```
I use a function getNeighbours that, for a given number, generates the list of possible neighbour coordinates and maps this list to the values in the grid. Then I simply need to check if any of these values is a symbol (ie not . nor number) to know that this number is adjacent to one.

I filter the numbers that are adjacent to a symbol, get their values and sum them together!

Part two is similar:
```hs
partTwo :: Input -> Output
partTwo (grid, numbers, gears) = sum . map (product . map value) . filter ((== 2) . length) . map getNeighbours $ gears
    where maxRow = nrows grid
          maxCol = ncols grid
          getNeighboursPos (r, c) = [(i, j) | i <- [r - 1 .. r + 1], j <- [c - 1 .. c + 1], 0 < i && i <= maxRow, 0 < j && j <= maxCol]
          getNeighbours g = filter (\(Number _ r (c1, c2)) -> any (`elem` neighbours) [(r, c) | c <- [c1 .. c2]]) numbers where neighbours = getNeighboursPos g
```
This time, I first generate the list of neighbouring coordinates, and the I filter the number list by checking if any matches these coordinates.

Then for each gear I keep those with exactly two neighbours, I get the value of these neighbours and multiply them, and I sum all the results.


I am not happy with that 😿
