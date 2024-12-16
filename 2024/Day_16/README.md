## Day 16

That was the craziest part 2 so far Owo

## The input

Once again, a grid of char, once again I used a Data.Array.

I am not going to explain much more how I parsed it, it's pretty similar to previous days,
especially [Day 14](../Day_14).

Note that I compiled the input inside a data structure:

```hs
data World = World { grid :: Array (Int, Int) Char, start :: (Int, Int), end :: (Int, Int) } deriving Show
```

```hs
parseInput :: String -> Input
parseInput input = World grid start end
    where gridLines = lines input
          (height, width) = (length gridLines, length $ head gridLines)
          grid  = listArray ((1, 1), (height, width)) $ filter (/= '\n') input
          start = fst . head . filter ((== 'S') . snd) . assocs $ grid
          end   = fst . head . filter ((== 'E') . snd) . assocs $ grid
```

## Part 1

### The problem

We need to find the length of the shortest path going from start to end.

### The solution

Let's use a shortest path algorithm from the [Algorithm.Search](https://hackage.haskell.org/package/search-algorithms-0.3.3/docs/Algorithm-Search.html) library.

I have decided to use dijkstra, it is well suited here because we can model our input as a weighted directed graph with positive weights.

In order to use that function we need to define a few things:
- What is a state in our graph
- What are the neighbouring state of a state
- What is the cost of going from one state to the other

I defined a state as a position and a direction:
```hs
data State = State { position :: (Int, Int), direction :: (Int, Int) } deriving (Show, Eq, Ord)
```

The neighbours of a state are:
- The next tile by going in the same direction
- The next tile by rotating 90 degrees
- The next tile by rotation -90 degrees

The tile has to be a non-wall tile inside the grid.

```hs
applyTuple :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
applyTuple f (a, b) (c, d) = (f a c, f b d)

getRotations :: (Int, Int) -> [(Int, Int)]
getRotations (di, dj) = [(di, dj), (dj, di), (-dj, -di)]


getNextStates :: Array (Int, Int) Char -> State -> [State]
getNextStates grid (State pos dir) = nexts
    where rotations = getRotations dir
          steps     = [State (applyTuple (+) pos d) d | d <- rotations]
          nexts     = filter ((/= '#') . (grid !) . position) steps
```

Now that I can get the neighbour of a tile, I need to define the cost function:

Assuming that s1 and s2 are neighbours
- If the two states have the same direction, then the cost of going from s1 to s2 is 1
- Otherwise it is 1001

```hs
getCost :: State -> State -> Int
getCost s1 s2 | direction s1 /= direction s2 = 1001
              | otherwise                    = 1
```

Now part 1 is just calling the dijkstra function with our model:
```hs
partOne :: Input -> Output
partOne (World arr start end) = fst . fromJust $ dijkstra getNexts getCost isDone (State start (0, 1))
    where getNexts = getNextStates arr
          isDone   = (== end) . position
```

## Part 2

### The problem

The path that the dijkstra function found is not the only shortest path.

Let's find all the tiles that are part of a shortest path.

### The solution

Omg.

Alright so, the easy way is to modify the dijkstra function to find all the shortest paths, as written
on the [Wikipedia page](https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm#Pseudocode)

However, I used a library, and I do not want to code Dijkstra in Haskell.

So what I did is the following:
- I go through my grid in a depth-first traversal
- My dfs will return me a Map, mapping states to a pair stating:
    - If this state is part of a shortest path
    - The smallest distance of that state from the start
  of the path.
- If the current distance from the start for that tile is greater than the smallest distance previously found, or that the current distance is greater than or equal to the shortest path's length, then we do not need to compute that state.
- If the current tile is the end tile, then we do not need to compute that tile. We can simply say that it is part
- Otherwise we visit the neighbours (and update our map accordingly). If one of the neighbour is part of the path, and that the shortest path for that neighbour is the distance of the current tile + the cost of going to that neighbour, then this means that our current tile is on a path.

```hs
          dfs x mem dist  | position x == end = Map.insert x (True, dist) mem
                          | dist >= bestDist = mem
                          | otherwise = mem''
            where (_, bestDist) = Map.findWithDefault (False, target) x mem
                  neighbours    = getNextStates arr x
                  costs         = map (getCost x) neighbours
                  tryVisit (next, cost) mem = dfs next mem (dist + cost)
                  isNeighbourOnPath cost (onPath, neighDist) = onPath && neighDist == dist + cost
                  mem' = foldr tryVisit mem (zip neighbours costs)
                  mem'' | or $ zipWith isNeighbourOnPath costs [Map.findWithDefault (False, target) n mem' | n <- neighbours] = Map.insert x (True, dist) mem'
                        | otherwise = Map.insert x (False, dist) mem'
```

Once we have that, we can filter the map to only get the states that are part of the path.

Then, we get the coordinates of these states (and we only keep unique ones), before counting how many tiles there are:

```hs
partTwo :: Input -> Output
partTwo input@(World arr start end) = length . nub . map position . Map.keys $ statesOnPath
    where target = partOne input
          states = dfs (State start (0, 1)) Map.empty 0
          statesOnPath = Map.filter fst states
          dfs x mem dist  | position x == end = Map.insert x (True, dist) mem
                          | dist >= bestDist = mem
                          | otherwise = mem''
            where (_, bestDist) = Map.findWithDefault (False, target) x mem
                  neighbours    = getNextStates arr x
                  costs         = map (getCost x) neighbours
                  tryVisit (next, cost) mem = dfs next mem (dist + cost)
                  isNeighbourOnPath cost (onPath, neighDist) = onPath && neighDist == dist + cost
                  mem' = foldr tryVisit mem (zip neighbours costs)
                  mem'' | or $ zipWith isNeighbourOnPath costs [Map.findWithDefault (False, target) n mem' | n <- neighbours] = Map.insert x (True, dist) mem'
                        | otherwise = Map.insert x (False, dist) mem'
```

## The end part

I have a feeling that I just recoded some path-finding algorithm, but to be honest I have no idea what I did here, if it even has a name.

In fact, this can solve both part one and two.

It seems to be a path-finding algorithm that works well when given a good higher-bound for distances.

One higher bound that can be easily found is 1001 * taxicab start end:
```hs
partBonus :: Input -> (Output, Output)
partBonus input@(World arr start end) = (distOfEnd, tilesOnPath)
    where distance (i, j) (k, l) = abs (i - k) + abs (j - l)
          maxDist = 1001 * distance start end
          states = dfs (State start (0, 1)) Map.empty 0
          statesOnPath = Map.filter fst states
          distOfEnd    = snd . Map.findMin . Map.map snd $ Map.filterWithKey (\k _ -> position k == end) states
          tilesOnPath  = length . nub . map position . Map.keys $ statesOnPath
          dfs x mem dist  | position x == end = Map.insert x (True, dist) mem
                          | dist >= bestDist  = mem
                          | otherwise         = mem''
            where (_, bestDist) = Map.findWithDefault (False, maxDist) x mem
                  neighbours    = getNextStates arr x
                  costs         = map (getCost x) neighbours
                  tryVisit (next, cost) mem = dfs next mem (dist + cost)
                  isNeighbourOnPath cost (onPath, neighDist) = onPath && neighDist == dist + cost
                  mem' = foldr tryVisit mem (zip neighbours costs)
                  mem'' | or $ zipWith isNeighbourOnPath costs [Map.findWithDefault (False, maxDist) n mem' | n <- neighbours] = Map.insert x (True, dist) mem'
                        | otherwise = Map.insert x (False, dist) mem'
```

Comparisons:

```
➜  Day_16 git:(main) ✗ time ./Day_16 one two input
134588
631
./Day_16 one two input  1.26s user 0.05s system 97% cpu 1.337 total
➜  Day_16 git:(main) ✗ time ./Day_16 bonus input
(134588,734)
./Day_16 bonus input  4.74s user 0.07s system 98% cpu 4.869 total
```

If anyone know what the hell I did, don't hesitate reaching me on [bluesky](https://bsky.app/profile/sheinxy.bsky.social).

