## Day 18

```py
from bfs import bfs

bfs()
```

## The input

The input is a list of walls.

Each line is a wall, described as a tuple.

```hs
parseInput :: String -> Input
parseInput = map parseLine . lines
    where parseLine line = read $ "(" ++ line ++ ")"
```

## Part 1

### The problem

We take the first 1024 walls. We want to find the shortest path going from (0, 0) to (70, 70).

### The solution

[This.](https://hackage.haskell.org/package/search-algorithms-0.3.3/docs/Algorithm-Search.html#v:bfs)

In order to use the bfs, I need to define 2 things:
- A function to generate the next states (the neighbours)
- A function to tell if the bfs is done

The next states are the neighbours that are not walls:
- They are the tiles with a taxicab distance of 1, inside the grid, that are not walls.

The bfs is done when the current state is the final tile.

```hs
numCorrupted :: Int
numCorrupted = 1024

size :: Int
size = 70

findPath :: Int -> Input -> Maybe [(Int, Int)]
findPath numWalls input = bfs getNextState isDone (0, 0)
    where walls = fromList $ take numWalls input
          getNextState (i, j) = filter (`notMember` walls) [(i + di, j + dj) | di <- [-1 .. 1],
                                                                               dj <- [-1 .. 1],
                                                                               abs di + abs dj == 1,
                                                                               0 <= i + di && i + di <= size,
                                                                               0 <= j + dj && j + dj <= size]
          isDone = (== (size, size))
```

Note that I'm using a [Data.Set](https://hackage.haskell.org/package/containers-0.7/docs/Data-Set.html) in order to lookup if a tile is a wall, as it is way faster than using `notElem` (O(log n) vs O(n)).

This means that, in the worst case scenario, for 1024 walls:
- With `notElem` each tile is going to go through the 1024 walls for each potential neighbour, so at most 1024 * 4 = 4096 times
- With `notMember`, we're looking at 10 walls (log2(1024)) 4 times, so 40 lookups for each tile.

That's a huge difference, trust me.

Now, we simply get the length of the path:
```hs
partOne :: Input -> Output
partOne = length . fromJust . findPath numCorrupted
```

## Part 2

### The problem

Now let's continue adding walls until there is no possible path.

### The solution

Bruteforce.

I start at 1024 walls, and I keep looking for a path until none is found.

```hs
partTwo :: Input -> (Int, Int)
partTwo input = input !! notBlocking
    where notBlocking = numCorrupted + (length . takeWhile isJust . map (`findPath` input) $ [numCorrupted + 1.. ])
```

## Bonus

### The problem

```
➜  Day_18 git:(main) ✗ time ./Day_18 one two input
276
(60,37)
./Day_18 one two input  35.58s user 0.72s system 97% cpu 37.140 total
```

That is slow, we can do better!

### The solution

The list of walls can be split into two parts:
- On one side, adding the walls won't block the path.
- On the other side, adding more walls won't unblock the path.

This calls for a dichotomic search!
- We have a lower bound that is always in the "not blocked" part
- We have a higher bound that is always in the "blocked" part
- We look at their middle point:
    - If it is in the "not blocked" part, then we update our lower bound
    - Otherwise we update the higher bound
- When there is no wall between the lower and higher bound, then this means the higher bound
 is the smallest possible higher bound, AKA our answer.

```hs
partBonus :: Input -> (Int, Int)
partBonus input = input !! notBlocking
    where numWalls = length input
          notBlocking = dichotomic numCorrupted numWalls - 1
          dichotomic low high | low + 1 == high = high
                              | isJust $ findPath m input  = dichotomic m high
                              | otherwise                  = dichotomic low m
            where m = (low + high) `div` 2
```

This is way faster!

```
➜  Day_18 git:(main) ✗ time ./Day_18 two input
(60,37)
./Day_18 two input  34.25s user 0.65s system 98% cpu 35.329 total
 ➜  Day_18 git:(main) ✗ time ./Day_18 bonus input
(60,37)
./Day_18 bonus input  0.06s user 0.01s system 17% cpu 0.417 total
```
