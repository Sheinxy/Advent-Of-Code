## Day 04

Repeat after me:

Manipulating 2D grids in Haskell isn't fun!

![Sobbing cat](https://gifdb.com/images/high/freaked-out-sad-cat-sobbing-x81xcmvrn4yyk9j9.gif)

Also, remember that I'm not going for the most optimised solution,
but for the one that doesn't require a lot of code!

## The input:

The input is a simple grid of characters, so we just get each line:

```
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
```

```hs
parseInput :: String -> Input
parseInput = lines
```

## Part 1:

### The Problem:

We need to search inside the grid for any "XMAS" horizontally, vertically and diagonally.

### The solution:

Searching horizontally is pretty easy:

We can simply use a regex search on "XMAS"!

However, we also need to find "SMAX", and using the regex library to search "XMAS|SMAX" won't
work for the case "XMASMAX" as the S is going to be taken by "XMAS" (Same with SMAXMAS). (Maybe there is a 
parameter for that, but I didn't find it :x)

One simple thing we can do to solve that is simply reversing the string before searching a second time!

```hs
search :: String -> [String]
search l = concat (l =~ "XMAS" :: [[String]])
searchHorizontal l = length $ concatMap (search . reverse) l ++ concatMap search l
```

Now that we can find all horizontal instances of "XMAS", searching for all vertical
instances is easy: simply rotate (transpose actually) the grid!:

```
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
->
MMAMXXSSMM
MSMSMXMAAX
MAXAAASXMM
SMSMSMMAMX
XXXAAMSMMA
XMMSMXAAXX
MSAMXXSSMM
AMASAAXAMA
SSMMMMSAMS
MAMXMASAMX
```

Now we simply have to search horizontally for all XMAS!

```hs
searchVertical     = searchHorizontal . transpose
```

Finally, the hardest part: counting diagonally.

In order to do that, I start by getting all subsets of 4 consecutive lines.

In order to do that, I make a chunksOf function that gets all subsets of n consecutives lines:
```hs
-- Not the same as Data.List.Split's chunksOf
-- This gives all the complete subsequences of length n
-- That is like iterating over take (length l - x) $ drop x l
chunksOf :: Int -> [a] -> [[a]]
chunksOf n = concatMap (filter ((== n) . length) . inits) . tails
```

For example, the second subset is the following:
```
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX

->

MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
```

Then, for each subset, I shift each line by it's index`.

For example our last result will get shifted like this:
```
MSAMXMSMSA
MXSXMAAMM
MASMSMX
AMXAMM
```

Now, if there was "XMAS" written diagonally, the letter would all fall on the same column!

In other terms, we now simply need to count vertically!

```hs
searchDiagonal  l  = countInDiagonal l + countInDiagonal (reverse l)
 where shift l         = [drop i x | (i, x) <- zip [0 .. ] l]
       getDiagonals    = map shift . chunksOf 4
       countInDiagonal = sum . map searchVertical . getDiagonals
```

If we put all that together:

```hs
partOne :: Input -> Output
partOne input = searchHorizontal input + searchVertical input + searchDiagonal input
    where search :: String -> [String]
          search l = concat (l =~ "XMAS" :: [[String]])
          searchHorizontal l = length $ concatMap (search . reverse) l ++ concatMap search l
          searchVertical     = searchHorizontal . transpose
          searchDiagonal  l  = countInDiagonal l + countInDiagonal (reverse l)
            where shift l         = [drop i x | (i, x) <- zip [0 .. ] l]
                  getDiagonals    = map shift . chunksOf 4
                  countInDiagonal = sum . map searchVertical . getDiagonals
```

And it's actually not to slow OwO
```
sh$ time ./Day_04 one input
2549
./Day_04 one input  0.04s user 0.01s system 11% cpu 0.473 total
```

## Part 2:

This part was actually simpler in my opinion :3c

### The problem:

Now, we don't actually want to find "XMAS" in the grid! We actually want
to find crosses that spell "MAS" twice!

For example:
```
M.S
.A.
M.A
```

### The solution

We can simply look through all 3x3 chunks in our grid, and check if it is
a 'MAS' cross.

There are actually only 4 valid 'MAS' crosses:
```
M.S S.M S.S M.M
.A. .A. .A. .A.
M.S S.M M.M S.S
```

So I harcoded them in a 'isCross' function:
```hs
isCross ['M', _, 'S'] [_, 'A', _] ['M', _, 'S'] = True
isCross ['S', _, 'M'] [_, 'A', _] ['S', _, 'M'] = True
isCross ['M', _, 'M'] [_, 'A', _] ['S', _, 'S'] = True
isCross ['S', _, 'S'] [_, 'A', _] ['M', _, 'M'] = True
isCross _ _ _                                   = False
```

Now, I get all possible chunks of 3 lines, and for each line I get all
the chunks of 3 columns.

I zip the column-chunks of each line to get all 3x3 chunks, and I apply isCross
on all of these chunks.

Now I filter the ones that are crosses, and I count their length:

```hs
partTwo :: Input -> Output
partTwo =  length . filter id . concatMap (zipWith3' isCross . map (chunksOf 3)) . chunksOf 3
    where zipWith3' f [a, b, c] = zipWith3 f a b c
          isCross ['M', _, 'S'] [_, 'A', _] ['M', _, 'S'] = True
          isCross ['S', _, 'M'] [_, 'A', _] ['S', _, 'M'] = True
          isCross ['M', _, 'M'] [_, 'A', _] ['S', _, 'S'] = True
          isCross ['S', _, 'S'] [_, 'A', _] ['M', _, 'M'] = True
          isCross _ _ _                                   = False
```

This still runs decently fast:
```
sh$ time ./Day_04 two input
2003
./Day_04 two input  1.04s user 0.03s system 97% cpu 1.095 total
```

## The end part

Of course, I could've used a Data.Matrix or some Data.Array, as they have fast
index-access functions.

And of course I could've then written a function that iterates through each tile
and look for 'XMAS' there in all 8 directions.

But that didn't feel right :3
