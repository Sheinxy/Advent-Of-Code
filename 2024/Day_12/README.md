## Day 12

Day 10 part 3 and 4, here we go

## The input

The input is a grid of characters. I parse it almost exactly as I did during
[Day 10](../Day_10), therefore I won't re-explain this.

```hs
type Input = Array (Int, Int) Char

parseInput :: String -> Input
parseInput input = listArray ((1, 1), (numRows, numCols)) (concat grid)
    where grid    = lines input
          numRows = length grid
          numCols = length . head $ grid
```

## Part 1

### The problem

We need to find regions, then for each region we multiply it's size (number of elements in it)
by it's perimeter.

### The solution

Finding regions is done using a BFS like during [Day 10](../Day_10),
the only differences are:
- An element is accessible if it is from the same region
- I check every starting position, and I launch the bfs on it if it isn't part
  of any previously visited region.

```hs
findRegions :: Input -> [Set (Int, Int)]
findRegions input = foldr visit [] (indices input)
    where visit idx vs
            | any (Set.member idx) vs  = vs
            | otherwise                = bfs [idx] (Set.singleton idx) : vs
          bfs [] v = v
          bfs (x : xs) visited = bfs xs' visited'
            where neighbours = getNeighbours input x
                  neighbours'= filter (`Set.notMember` visited) neighbours
                  current    = input ! x
                  accessible = filter ((== current) . (input !)) neighbours'
                  xs'        = xs ++ accessible
                  visited'   = foldr Set.insert visited accessible
```

Once I have my regions, I can find the border size of each tile by checking the number of empty tiles
around it. My perimeter is the sum of these border sizes:

```hs
computePrice :: Set (Int, Int) -> Int
computePrice region = Set.size region * Set.foldr (\x acc -> acc + getBorderSize x) 0 region
    where getBorderSize (i, j) = length [(i + di, j + dj) | di <- [-1 .. 1],
                                                            dj <- [-1 .. 1],
                                                            abs di + abs dj == 1,
                                                            (i + di, j + dj) `Set.notMember` region]
```


## Part 2

This is where things get interesting.

### The problem

I want to find the number of edges of each region. This is harder that finding the perimeter.


### The solution

Weird solution, typical for me.

I count the number of turning corners:
- A turn is composed of two edges, so I simply need to multiply the number of corners by two.

The problem here is that I need to be careful about how I count my corners, as I might end up counting edges twice:

```
AA
AA
```

For example here, if I count "F"-shaped corners, "L"-shaped corners, "7"-shaped corners and "J"-shaped corners,
I find four corners. Which is technically true, but I would now need to find which edge overlaps with which.

Therefore, I will only count "complimentary" corners. I have decided to only count "F" and "J" corners.

The problem now is with the following shape:
```
AAAA
AXXA
AAAA
```

Here I would only find two corners!

This is because I also want to count "inner" corners.

I have decided to count the "L" and "7" inner corners, that way they don't clash with the outer corners:
```
...A
AAAA
A...
A...
```
If I counted inner "F" corners here, the inner F corner would overlap with the outer Js.

A tile might corresponds to multiple corners (however it is either an outer corner or an inner one, but that doesn't matter much).

```hs
computeBulkPrice :: Set (Int, Int) -> Int
computeBulkPrice region = sum (map countEdgesOfTurn turnPoints) * Set.size region
    where -- These are the corners (L7FJ) of the shape.
          turnPoints = Set.toAscList $ Set.filter isTurnPoint region

          -- This tests if a point is a corner. F and J are outer corners, while L and 7 are inner corners.
          -- This is done in order to count each edge only once.
          isOutFTurnPoint (i, j) = all (`Set.notMember` region) [(i - 1, j), (i, j - 1)]
          isOutJTurnPoint (i, j) = all (`Set.notMember` region) [(i + 1, j), (i, j + 1)]
          isInLTurnPoint  (i, j) = (i - 1, j + 1) `Set.notMember` region && all (`Set.member` region) [(i - 1, j), (i, j + 1)]
          isIn7TurnPoint  (i, j) = (i + 1, j - 1) `Set.notMember` region && all (`Set.member` region) [(i + 1, j), (i, j - 1)]
          turnPointList          = [isOutFTurnPoint, isOutJTurnPoint, isInLTurnPoint, isIn7TurnPoint]

          isTurnPoint x          = any ($ x) turnPointList
          countEdgesOfTurn x     = 2 * length $ filter ($ x) turnPointList

partTwo :: Input -> Output
partTwo = sum . map computeBulkPrice . findRegions
```

## The end part

What the heck did I do
