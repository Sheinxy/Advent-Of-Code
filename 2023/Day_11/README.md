## Day 11

Today was an easy day (thanks god, I wouldn't be able to endure two day 10 in a row)

Here is my solution:
```hs
data Input  = Input  { galaxies :: [(Int, Int)], emptyRows :: [Int], emptyCols :: [Int] } deriving (Show)
type Output = Int

parseInput :: String -> Input
parseInput input = Input galaxies emptyRows emptyCols
    where rows      = lines input
          ncols     = length . head $ rows
          galaxies  = [(i, j) | (i, row) <- zip [1 .. ] rows, (j, char) <- zip [1 .. ] row, char == '#']
          emptyRows = [i      | (i, _)   <- zip [1 .. ] rows, i `notElem` map fst galaxies]
          emptyCols = [j      |  j       <-     [1 .. ncols], j `notElem` map snd galaxies]

getRealCoordinates :: Input -> Int -> [(Int, Int)]
getRealCoordinates (Input g r c) coef = map go g
    where go (row, col) = (row + (coef - 1) * rowsBefore, col + (coef - 1) * colsBefore)
            where rowsBefore = length . takeWhile (< row) $ r
                  colsBefore = length . takeWhile (< col) $ c

dist :: (Int, Int) -> (Int, Int) -> Int
dist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

solve :: Int -> Input -> Int
solve coef input = sum [dist a b | a <- coordinates, b <- coordinates, a < b]
    where coordinates = getRealCoordinates input coef

partOne :: Input -> Output
partOne = solve 2

partTwo :: Input -> Output
partTwo = solve 1000000
```

## The input:

```hs
data Input  = Input  { galaxies :: [(Int, Int)], emptyRows :: [Int], emptyCols :: [Int] } deriving (Show)
type Output = Int

parseInput :: String -> Input
parseInput input = Input galaxies emptyRows emptyCols
    where rows      = lines input
          ncols     = length . head $ rows
          galaxies  = [(i, j) | (i, row) <- zip [1 .. ] rows, (j, char) <- zip [1 .. ] row, char == '#']
          emptyRows = [i      | (i, _)   <- zip [1 .. ] rows, i `notElem` map fst galaxies]
          emptyCols = [j      |  j       <-     [1 .. ncols], j `notElem` map snd galaxies]
```

I parse my input into a data structure containing:
 - The list of galaxies (represented by their coordinates)
 - The list of empty rows (represented by their index)
 - The lis of emtpy columns (represented by their index as well)

In order to parse my input, I simply start by spliting it by lines (which gives me my rows).
I find the coordinates of every '#' character and I keep it in my list of galaxies.

To find the empty rows, I go over each number from 1 to the number of rows, and I check if that number corresponds to a galaxy's row in my list of galaxies.

I do exactly the same thing with columns to find my empty columns.


## Solving:

```hs
getRealCoordinates :: Input -> Int -> [(Int, Int)]
getRealCoordinates (Input g r c) coef = map go g
    where go (row, col) = (row + (coef - 1) * rowsBefore, col + (coef - 1) * colsBefore)
            where rowsBefore = length . takeWhile (< row) $ r
                  colsBefore = length . takeWhile (< col) $ c

dist :: (Int, Int) -> (Int, Int) -> Int
dist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

solve :: Int -> Input -> Int
solve coef input = sum [dist a b | a <- coordinates, b <- coordinates, a < b]
    where coordinates = getRealCoordinates input coef
```

Solving uses three main functions:
 - getRealCoordinates which tranforms my galaxies input coordinates into their real coordinates after expanding empty rows and columns by the coef value.
 - dist, which is a taxicab distance between two points
 - solve which first gets the real galaxy coordinates from the input, and then compute the distance between every pair of point (without duplicating each pair by ordering it), and then sum all the distances.

To get the real coordinates of a galaxy, I simply look at the number of empty rows/columns that are before that galaxy (I do this using length . takeWhile(< row/col), as the list of empty rows/columns is already sorted. For example is my empty rows are [1, 4, 5, 7, 8] and my galaxy is (6, 2), then the takeWhile (< row) will yield [1, 4, 5], so there are three empty rows before my galaxy).

Then to find the real coordinates, I simply multiply the number of empty rows/columns by (coef - 1) and I add that to my current row/column.

(For the sake of simplicity, I am now only going to focus on rows, but this principle holds for columns)

The reason I multiply by (coef - 1) is the following:

If a row expands by, let's say, n. Then we can think of it as adding (n - 1) new rows after it.
For example, let's expand the following with a coef of 3:

```
...........
#..........
```

Would give:
```
...........
...........
...........
#..........
```

We added 2 new rows!

Now we need to add (coef - 1) rows for each empty row, therefore we multiply (coef - 1) by the number of empty rows.

Now to solve the actual puzzles, I simply need to call my solve function with the right parameters:
```hs
partOne :: Input -> Output
partOne = solve 2

partTwo :: Input -> Output
partTwo = solve 1000000
```

And voilà!
