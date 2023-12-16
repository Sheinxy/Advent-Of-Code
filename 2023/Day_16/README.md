## Day 16

Today I woke up at 6AM to try to see how many points I could score.

I lost 20 minutes debugging my code because I swapped North and South somewhere in my code. 😸

Here is my solution:

```hs
data Direction = North | South | East | West deriving (Show, Eq, Ord)

data Move = Move { position :: (Int, Int), direction :: Direction } deriving (Show, Eq, Ord)

type Input = Matrix Char
type Output = Int

parseInput :: String -> Input
parseInput = fromLists . lines

-- Will clean that up later maybe
getNexts :: Move -> Input -> [Move]
getNexts (Move (r, c) North) grid | char `elem` ".|" = [Move (r - 1, c    ) North]
                                  | char == '/'      = [Move (r    , c + 1) East ]
                                  | char == '\\'     = [Move (r    , c - 1) West ]
                                  | char == '-'      = [Move (r    , c - 1) West, Move (r, c + 1) East]
                                  where char = grid ! (r, c)

getNexts (Move (r, c) South) grid | char `elem` ".|" = [Move (r + 1, c    ) South ]
                                  | char == '\\'     = [Move (r    , c + 1) East  ]
                                  | char == '/'      = [Move (r    , c - 1) West  ]
                                  | char == '-'      = [Move (r    , c - 1) West, Move (r , c + 1) East]
                                  where char = grid ! (r, c)

getNexts (Move (r, c) East) grid  | char `elem` ".-" = [Move (r    , c + 1) East  ]
                                  | char == '\\'     = [Move (r + 1, c    ) South ]
                                  | char == '/'      = [Move (r - 1, c    ) North ]
                                  | char == '|'      = [Move (r - 1, c    ) North, Move (r + 1, c) South]
                                  where char = grid ! (r, c)

getNexts (Move (r, c) West) grid  | char `elem` ".-" = [Move (r    , c - 1) West  ]
                                  | char == '\\'     = [Move (r - 1, c    ) North ]
                                  | char == '/'      = [Move (r + 1, c    ) South ]
                                  | char == '|'      = [Move (r - 1, c    ) North, Move (r + 1, c) South]
                                  where char = grid ! (r, c)

bfs :: Set Move -> [Move] -> Input -> Int
bfs seen []     _    = size . S.map position $ seen
bfs seen (x:xs) grid = bfs seen' queue grid
    where nexts   = getNexts x grid
          inGrid  = filter (\(Move (r, c) _) -> 0 < r  && r <= nrows grid && 0 < c && c <= ncols grid) nexts
          notSeen = filter (`notMember` seen) inGrid
          seen'   = foldr insert seen notSeen
          queue   = xs ++ notSeen

partOne :: Input -> Output
partOne = bfs (singleton (Move (1, 1) East)) [Move (1, 1) East]

partTwo :: Input -> Output
partTwo grid = maximum possibilities
    where nr     = nrows grid
          nc     = ncols grid
          starts = [Move (1 , col) South | col <- [1 .. nc]] ++ [Move (row, 1 ) East | row <- [1 .. nr]] ++
                   [Move (nr, col) North | col <- [1 .. nc]] ++ [Move (row, nc) West | row <- [1 .. nr]]
          launch mv     = bfs (singleton mv) [mv] grid
          possibilities = parMap rseq launch starts
```

You know the drill by now, let's start!

## The input:

Once again, the puzzle involves a 2D grid. By now you should know that I love using [Data.Matrix](https://hackage.haskell.org/package/matrix-0.3.6.1/docs/Data-Matrix.html) when dealing with 2D grids!

To parse my input, I simply split by lines and transform my list of strings into a matrix of chars

```hs
type Input = Matrix Char

parseInput :: String -> Input
parseInput = fromLists . lines
```

## Beam tracing

I divided my beam tracing algorithm into two parts
 - Knowing where the beam should go next from its current position and heading
 - A [Breadth-first traversal](https://en.wikipedia.org/wiki/Breadth-first_search) of the grid for tracing the beam

### Where to go now?

In order to trace my beam, I have two structures:
 - Direction, which self-explanatory
 - Move, which represents the current position of the beam and its current heading. So Move (1, 1) East means that the beam is on tile (1, 1) and came here by moving Eastbound

```hs
data Direction = North | South | East | West deriving (Show, Eq, Ord)
data Move = Move { position :: (Int, Int), direction :: Direction } deriving (Show, Eq, Ord)
```

In order to know the next position of the beam I did the simplest thing I could do:
 - I basically hard translated the puzzle's description into code:

```hs
-- Will clean that up later maybe
getNexts :: Move -> Input -> [Move]
getNexts (Move (r, c) North) grid | char `elem` ".|" = [Move (r - 1, c    ) North]
                                  | char == '/'      = [Move (r    , c + 1) East ]
                                  | char == '\\'     = [Move (r    , c - 1) West ]
                                  | char == '-'      = [Move (r    , c - 1) West, Move (r, c + 1) East]
                                  where char = grid ! (r, c)

getNexts (Move (r, c) South) grid | char `elem` ".|" = [Move (r + 1, c    ) South ]
                                  | char == '\\'     = [Move (r    , c + 1) East  ]
                                  | char == '/'      = [Move (r    , c - 1) West  ]
                                  | char == '-'      = [Move (r    , c - 1) West, Move (r , c + 1) East]
                                  where char = grid ! (r, c)

getNexts (Move (r, c) East) grid  | char `elem` ".-" = [Move (r    , c + 1) East  ]
                                  | char == '\\'     = [Move (r + 1, c    ) South ]
                                  | char == '/'      = [Move (r - 1, c    ) North ]
                                  | char == '|'      = [Move (r - 1, c    ) North, Move (r + 1, c) South]
                                  where char = grid ! (r, c)

getNexts (Move (r, c) West) grid  | char `elem` ".-" = [Move (r    , c - 1) West  ]
                                  | char == '\\'     = [Move (r - 1, c    ) North ]
                                  | char == '/'      = [Move (r + 1, c    ) South ]
                                  | char == '|'      = [Move (r - 1, c    ) North, Move (r + 1, c) South]
                                  where char = grid ! (r, c)
```

There really isn't much more to say, but I'm going to give an example to make sure you understand.

Let's say that my current move is Move (4, 5) West. This means that my beam was previously on (4, 6) and is now on (4, 5) because it move Westbound. Now, let's say that at (4, 5) the tile is a '|' tile. According to the puzzle, my beam should split in two: one moving Northbound and one moving Southbound. Therefore the next two moves are going to be on tile (3, 5) moving Northbound and tile (5, 5) moving Southbound.

##### This is were I lost 20 minutes, because I swapped North and South for Westbounds and Eastbounds moves. I said that '\\' would give a Southbound move when coming from West etc.

### Tracing the beam! (Not to be mistaken for Racing the beam)

In order to simulate one beam, I simply use a bfs:

```hs
bfs :: Set Move -> [Move] -> Input -> Int
bfs seen []     _    = size . S.map position $ seen
bfs seen (x:xs) grid = bfs seen' queue grid
    where nexts   = getNexts x grid
          inGrid  = filter (\(Move (r, c) _) -> 0 < r  && r <= nrows grid && 0 < c && c <= ncols grid) nexts
          notSeen = filter (`notMember` seen) inGrid
          seen'   = foldr insert seen notSeen
          queue   = xs ++ notSeen
```

I have a set of Move which keeps track of the moves I've already done, because one move will always yield the same results, therefore I do not want to repeat moves (otherwise I would go forever).

When my queue is empty, I simply get the tiles I've visited from my set of moves (no longer caring about the direction I visited them from, so Move (2, 4) East and Move (2, 4) West will both count as one tile only, (2, 4)), and I count how many tiles I've visited.

When it isn't empty, I get the next moves for my current move, and I filter them to make sure they are both valid (ie in the grid) and new moves (ie not in my set). I append these moves to my queue, and I also mark them as seen.

Now for part one, I simply need to launch my traversal, starting from (1, 1) going Eastbound (with that move marked as visited):

```hs
partOne :: Input -> Output
partOne = bfs (singleton (Move (1, 1) East)) [Move (1, 1) East]
```

### Tracing the beam**s**!

Part 2 was not really complicated, it's just launching the traversal from every tile on the edge of the grid and taking the best result. The only neat thing that I did was that I parallelised it so that traversals would run simultaneously to go faster.

```hs
partTwo :: Input -> Output
partTwo grid = maximum possibilities
    where nr     = nrows grid
          nc     = ncols grid
          starts = [Move (1 , col) South | col <- [1 .. nc]] ++ [Move (row, 1 ) East | row <- [1 .. nr]] ++
                   [Move (nr, col) North | col <- [1 .. nc]] ++ [Move (row, nc) West | row <- [1 .. nr]]
          launch mv     = bfs (singleton mv) [mv] grid
          possibilities = parMap rseq launch starts
```

That's all folks! 🐈‍⬛

Once again, if you have question just ask me!
