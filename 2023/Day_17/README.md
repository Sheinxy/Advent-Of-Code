## Day 17

Ah yes, [my best nightmare](https://www.youtube.com/watch?v=SObWQRaltug): Pathfinding with weights on a 2D grid in Haskell. 🐈‍⬛

## The input:
Let's start with the simple part: Parsing the input

```hs
type Input = Matrix Int

parseInput :: String -> Input
parseInput = fromLists . map (map digitToInt) . lines
```

You know me by now: when I see a 2D grid, I use [Data.Matrix](https://hackage.haskell.org/package/matrix-0.3.6.1/docs/Data-Matrix.html) (it's simple to use, it has O(1) access to elements, and it has some other useful utility functions)

So here I represent my input as a Matrix of Int. To parse my input file, I simply split its content by lines. For each character on each line, I get the corresponding digit as an integer.
I transform my list of lists of integers into a Matrix and I'm done.

## States:

Before actually coding my solution, I first want to define my states. A state here is a data structure containing:
 - A position
 - A direction
 - A number of consecutive moves in that direction

```hs
data Direction = North | South | East | West deriving (Show, Eq, Ord)
data Move = Move { position :: (Int, Int), direction :: Direction, consecutive :: Int } deriving (Show, Eq, Ord)
```

For example: Move (2, 1) South 1 means that we currently are on the tile (2, 1), we came here by going South and this was the first time moving South in a row

## [Rolling around at the speed of sound](https://www.youtube.com/watch?v=qR6mTJRbcaE):

Now one next thing to be able to do would be to know how to go from one state to another.

That means:
 - Knowing what moving in a direction means for a position (for example: what does moving north means when the position was (4, 5)?)
 - Knowing what directions I can move towards if my current state is about moving in one direction

I started by defining two useful functions:
```hs
opposite :: Direction -> Direction
opposite North = South
opposite South = North
opposite East  = West
opposite West  = East

addDirection :: Direction -> (Int, Int) -> (Int, Int)
addDirection North = first  (-1 +)
addDirection South = first  ( 1 +)
addDirection East  = second ( 1 +)
addDirection West  = second (-1 +)
```

Opposite does exactly what its name suggest: it takes a direction and returns its opposite.
This is useful because we cannot move towards North is we were heading towards South for example.

Add Direction takes a direction and a position and returns the new position we're at when moving in that direction:
```hs
addDirection North (4, 5) == (3, 5)
```

With these utility functions, I am able to create a getNeighbours functions that takes a state and returns its neighbouring states (ie. the states that can be moves towards from that state):

```hs
getNeighbours :: Int -> Int -> Move -> [Move]
getNeighbours min max (Move pos dir consec) | mustKeepDirection = filter ((== dir) . direction) neighbours
                                            | otherwise         = neighbours
                   where neighbours = filter ((/= (1, 1)) . position)                       . -- Do not go back to the starting point, Ever. It is useless. Bad boy.
                                      filter ((<= max)    . consecutive)                    . -- Do not try moves that would imply moving too much in one direction
                                      map (\x -> Move (addDirection x pos) x (newConsec x)) . -- Get the move state (position, direction and number of consecutive moves)
                                      filter (/= opposite dir) $ [North, South, East, West]   -- Try moves in all possible directions, excepting reverse

                         mustKeepDirection = 0 < consec && consec < min                       -- If we already move once in a direction, but not the minimum number of times, then we need to keep going

                         -- Getting the new number of consecutive moves for a move in the new direction
                         newConsec ndir | consec <= 0 = consec + 1 -- If there was no move yet, then this move is the first move (useful for the starting case)
                                        | ndir == dir = consec + 1 -- If we're still going in the same direction, then this is the consec + 1 move
                                        | otherwise   = 1          -- This is the first move in that direction
```

This is a bit heavy to explain, but I will try my best.

First of all, note that, aside from the current Move state, this function also takes two other parameters:
 - min is the minimum number of times we must move in a direction. Basically, if min is 4 then we need to move at least 4 consecutive times in a row towards the same direction before being able to pick a new one
 - max is the maximum number of times we can move towards the same direction. For example, if max is 10 then we cannot move 11 times in a row in the same direction. We need to change direction after the 10th move.

To know what our neighbouring states are, let's start by removing the forbidden direction. For example, if our current state was achieved by moving North, then we are not allowed to move South. This is what the filter (/= opposite) dire is about.

Then, we create the different states by getting the new position and the new number of consecutive steps taken for each allowed direction:
    - If our current state is Move (4, 5) North 1, then we get the states [Move (3, 5) North 2, Move (4, 6) East 1, Move (4, 4) West 1]

We remove any invalid state. A state is invalid if:
 - The maximum number of consecutive moves in that direction has been surpassed
 - Or if we're trying to go back to the starting tile (which isn't invalid per se, just useless to do)

Now the last check we do is:
 - Were we already moving in a direction, and should we keep going?

If that is the case (ie the minimum number of consecutive moves in that direction has not been reached), then we just keep the next move going in the same direction.

With that done, we now have a function taking a current state and returnin the next accessible states!

## [Jump Up, Super AStar](https://www.youtube.com/watch?v=1bZtCt_Siro):

All of that is cool, but we're yet to have solved anything... 😽

What we really want to do here is find the minimum heat loss number we can get going from the top left to the bottom right of the grid.

We can think of this as a simple find the shortest path problem, were the distances are represented by the heat lost of each tile.
And who says "Shortest path finding algorithm with weights" usually says [Dijkstra](https://en.wikipedia.org/wiki/Dijkstra's_algorithm)!

But I was NOT going to code Dijkstra in Haskell all by myself with my sleep deprived brain at 6AM. No can do! 😸 So I had two choices here:
 - Go back to sleep and do that when I'm more awake
 - Persist and find a library with a generic Dijkstra function

I did the second, and found this [wonderful library](https://hackage.haskell.org/package/search-algorithms)

After playing around with it, I decided not to use its Dijkstra function, but instead use its [A\*](https://en.wikipedia.org/wiki/A*_search_algorithm) function which ran just a little bit faster with the right heuristic.

```hs
-- findMinHeatLoss (minimum number of moves) (maximum number of moves) (input grid) -> minimum heat loss
findMinHeatLoss :: Int -> Int -> Input -> Output
findMinHeatLoss min max grid = fst . fromJust . aStar getNexts getCost heuristic isTarget $ start
    where getNexts     = getNeighbours min max `pruning` (not . isInGrid) -- Get next possible states for a move state (ie. neighbours in grid that don't break the move rules)
          getCost _ ns = grid ! position ns                               -- Get the cost of moving to a neighbour     (ie. the value of the neighbour in the grid)
          isTarget m   = position m == end && consecutive m >= min        -- Is the current state our target state?    (ie. is it at the end of the grid, and did we fit the move rules to get there?)
          heuristic    = dist end . position                              -- Heuristic for aStar: The best path will be the shortest, weights not accounted.

          end        = (nrows grid, ncols grid) -- Our end goal
          start      = Move (1, 1) East 0       -- Our starting state (Tile (1, 1) with a random direction and 0 moves yet)

          isInGrid mv = 0 < row && row <= nrows grid && 0 < col && col <= ncols grid where (row, col) = position mv -- Is the Move state in the grid?
          dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2) -- Standard taxicab distance for heuristic
```

The library's aStar function takes 5 arguments:
 - The function to generate list of neighboring states given the current state: our getNeighbours, to which I associate a bound checking to keep our neighbours in the grid)
 - The function to generate transition costs between neighboring states: here the cost is the value of the next position in the grid. the cost doesn't really depend on the current state
 - The estimate on remaining cost given a state AKA our heuristic: here our heuristic is that the cost of a position is the taxicab distance to go to our end target from that position. (basically, assume that all costs are equal)
 - The predicate to determine if solution found: is the current state position our end target, and did we get here in a valid number of consecutive steps?
 - The initial state: Move (1, 1) East 0. Out initial position is (1, 1) as it is the top left corner of the grid. Our initial direction is East as it allows us to move either East or South and our number of consecutive moves Eastbounds is 0 as we have yet to move.

Calling the aStar function with those arguments yields a tuple (wrapped around a Maybe monad, in case the search didn't find a result, which is a case we don't care about here so we unwrap it with fromJust):
 - The total distance to go from initial state to target state: ie the answer to our puzzle
 - The path taken: not useful here but might be cool for some visualisations!

Now we simply need to call that function with the right minimum and maximum number of moves in the same direction to get both our answers:

```hs
partOne :: Input -> Output
partOne = findMinHeatLoss 0 3

partTwo :: Input -> Output
partTwo = findMinHeatLoss 4 10
```

And we're done! Thanks for reading! 🐈‍⬛
