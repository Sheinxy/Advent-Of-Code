## Day 10

When part 2 is exactly like part 1 but with less things to handle

![Confused cat](https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Fyt3.ggpht.com%2Fa-%2FAAuE7mAo5osHnoXHx_W7kzrExr9peceVQ-VG2qAkKA%3Ds900-mo-c-c0xffffffff-rj-k-no&f=1&nofb=1&ipt=b602fe3c32f60ee99ef4d462ce673176438e5e2032867df5541c5ad3f72ff9d2&ipo=images)

## The input

Once again, we have a grid.

Once again, I handle things differently.


Today I have decided to finally use a Data.Array to represent my grid!

```hs
type Input = Array (Int, Int) Int
parseInput :: String -> Input
parseInput input = listArray ((1, 1), (numRows, numCols)) (concat grid)
    where grid    = map (map digitToInt) $ lines input
          numRows = length grid
          numCols = length . head $ grid
```

This is an array indexed with (Int, Int) tuples (coordinates).

## Part 1

### The problem

We have to find all the 0-tiles that can lead to a 9-tile by moving in four directions,
always increasing by 1.

### The solution

This is a textbook example of a graph search. There are two types of graph search:
[BFS](https://en.wikipedia.org/wiki/Breadth-first_search) and [DFS](https://en.wikipedia.org/wiki/Depth-first_search).

DFS is technically more suitable here, as it is intrinsically recursive, however I instinctively
went with a BFS here because my mind associates grids with BFS.

In both case, we first need to define a `getNeighbours` function.

Here, neighbours of a tile are in-bounds tiles within a [taxicab distance](https://en.wikipedia.org/wiki/Taxicab_geometry) of 1:
```hs
getNeighbours :: Input -> (Int, Int) -> [(Int, Int)]
getNeighbours grid (i, j) = filter isInBound neighbours
    where ((minRow, minCol), (maxRow, maxCol)) = bounds grid
          isInBound (i, j) = minRow <= i && i <= maxRow && minCol <= j && j <= maxCol
          neighbours       = [(i + di, j + dj) | di <- [-1 .. 1], dj <- [-1 .. 1], abs di + abs dj == 1]
```

Once we have that, we find the starting points in our grid. That is, the 0-tiles:
```hs
    where startingPoints = map fst . filter ((== 0) . snd) . assocs $ input
```

Then, we define the bfs function that works this way:
- If the queue is empty, we return 0 (this trail doesn't lead to any 9)
- If the current element in the queue is a 9 we count this trail and we keep visiting the queue
- Otherwise we add the non-visited neighbours to the queue and we keep visiting.
  A neighbour is considered visited once it is put in the queue. This is essential in order
  to **count each trail exactly once.**

```hs
          bfs [] _ = 0
          bfs (x : xs) visited
            | current == 9   = 1 + bfs xs visited
            | otherwise      = bfs xs' visited'
            where neighbours = getNeighbours input x
                  neighbours'= filter (`Set.notMember` visited) neighbours
                  current    = input ! x
                  accessible = filter ((== current + 1) . (input !)) neighbours'
                  xs'        = xs ++ accessible
                  visited'   = foldr Set.insert visited accessible
```

Finally, we sum all the bfs using the starting points:

```hs
partOne :: Input -> Output
partOne input = sum [bfs [p] $ Set.singleton p | p <- startingPoints]
    where startingPoints = map fst . filter ((== 0) . snd) . assocs $ input
          bfs [] _ = 0
          bfs (x : xs) visited
            | current == 9   = 1 + bfs xs visited
            | otherwise      = bfs xs' visited'
            where neighbours = getNeighbours input x
                  neighbours'= filter (`Set.notMember` visited) neighbours
                  current    = input ! x
                  accessible = filter ((== current + 1) . (input !)) neighbours'
                  xs'        = xs ++ accessible
                  visited'   = foldr Set.insert visited accessible
```

## Part 2

### The problem

We no longer want to count each 9-tile exactly once.

### The solution

Let's remove the part that was essential to count the trails exactly once:

```hs
partTwo :: Input -> Output
partTwo input = sum [bfs [p] | p <- startingPoints]
    where startingPoints = map fst . filter ((== 0) . snd) . assocs $ input
          bfs [] = 0
          bfs (x : xs) 
            | current == 9   = 1 + bfs xs
            | otherwise      = bfs xs'
            where neighbours = getNeighbours input x
                  current    = input ! x
                  accessible = filter ((== current + 1) . (input !)) neighbours
                  xs'        = xs ++ accessible
```

Done!

## The end part

That was a bit surprising. The fun fact is that I accidentally did part 2 first because
I forgot to count each 9 exactly once.
