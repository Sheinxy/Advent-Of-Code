## Day 15

2D grid manipulation day :x

## The input

The input is separated in two parts: a 2D grid and a list of directions.

I start by creating a data type describing the current state of my grid,
it contains:
- The grid itself
- The position of the robot

```hs
data World = World { grid :: Array (Int, Int) Char, position :: (Int, Int) } deriving Show
```

I separate my input when there is two consecutive line breaks. I retrive the grid
from the first part as an array indexed with pair of coordinates (starting at 0), and I retrieve the
coordinated of the robot represented by @:

```hs
type Input = (World, [Char])

parseInput :: String -> Input
parseInput input = (World grid start, moves)
    where [gridStr, moveLines] = splitOn "\n\n" input
          moves                = filter (/= '\n') moveLines
          gridLines            = lines gridStr
          height               = length gridLines
          width                = length $ head gridLines
          grid                 = listArray ((0, 0), (height - 1, width - 1)) $ filter (/= '\n') gridStr
          start                = fst . head . filter ((== '@') . snd) . assocs $ grid
```

## Solving the problem

I want to be able to move my robot by following the directions.

If the robot moves into a box, it should push that box, and propagate that movement to other boxes behind the pushed box.

If this results in trying to push the box into a wall, then no movement happens.

In order to move around, I first need a function describing a movement given a direction:

```hs
getStep :: Char -> (Int, Int)
getStep '<' = (0, -1)
getStep '>' = (0,  1)
getStep 'v' = (1,  0)
getStep '^' = (-1, 0)
```

I also made a helper function to apply that movement to given coordinates:
```hs
applyTuple :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
applyTuple f (a, b) (c, d) = (f a c, f b d)
```

Using this function, getting the next position of the robot can be done like that:
```hs
applyTuple (+) (position world) (getStep direction)
```

Now, what I need to do is to propagate this movement across boxes:

- If the new tile is a wall or an empty tile, I do not need to propagate from that tile
- If the new tile is a regular box ('O'), or part of a box ('[' or ']') and we're moving horizontally, I only need to propagate in the current direction
- If the new tile is the right part of a box, I need to propagate in the current direction and on the left part of the box as well
- If the new tile is the left part of a box, I need to propagate in the current direction and on the right part of the box as well

I perform this computation in a BFS way:
```hs
          -- Propagates the movement along boxes
          propagateStep [] v = Set.toAscList v
          propagateStep (x : xs) v
            | current `elem` "#." = propagateStep xs v
            | otherwise           = propagateStep xs' v'
            where current = g ! x
                  neighbours | dir `elem` "v^" && current == '[' = nub [applyTuple (+) x (0, 1), applyTuple (+) x step]
                             | dir `elem` "v^" && current == ']' = nub [applyTuple (-) x (0, 1), applyTuple (+) x step]
                             | otherwise      = [applyTuple (+) x step]
                  neighbours' = filter (`Set.notMember` v) neighbours
                  xs'         = xs ++ neighbours'
                  v'          = foldr Set.insert v neighbours'
```

Now that I have found the tiles that are going to be updated, I need to find their new state:
- If the tile that comes by going in the opposite direction was also an updated tile, then the current tile's state is that of the previous one, i.e: this tile is the result of pushing the box from the previous tile
- otherwise, this tile had a box on it that is now pushed, and is therefore an empty tile.
```hs
          -- Get a tile's new state
          getNew p | prev `elem` moves = g ! prev -- If the previous tile was moved, then this tile's new state is the previous tile's 
                   | otherwise         = '.'      -- Otherwise, this tile is now empty
                   where prev = applyTuple (-) p step
```

With that done, all I need to do is to update the grid accordingly:
- If any of the tiles to update is a wall, then the move is impossible and therefore no movement should happen
- Otherwise I simply need to update the tiles according to the rule previously stated

````hs
move :: World -> Char -> World
move world dir
        | any ((== '#') . (g !)) moves = world
        | otherwise                    = World g' pos'
    where step  = getStep dir
          g     = grid world
          pos   = position world
          pos'  = applyTuple (+) pos step
          moves = pos : propagateStep [pos'] (Set.singleton pos')

          g'    = g // [(p, getNew p ) | p <- moves]

          -- Get a tile's new state
          getNew p | prev `elem` moves = g ! prev -- If the previous tile was moved, then this tile's new state is the previous tile's 
                   | otherwise         = '.'      -- Otherwise, this tile is now empty
                   where prev = applyTuple (-) p step

          -- Propagates the movement along boxes
          propagateStep [] v = Set.toAscList v
          propagateStep (x : xs) v
            | current `elem` "#." = propagateStep xs v
            | otherwise           = propagateStep xs' v'
            where current = g ! x
                  neighbours | dir `elem` "v^" && current == '[' = nub [applyTuple (+) x (0, 1), applyTuple (+) x step]
                             | dir `elem` "v^" && current == ']' = nub [applyTuple (-) x (0, 1), applyTuple (+) x step]
                             | otherwise      = [applyTuple (+) x step]
                  neighbours' = filter (`Set.notMember` v) neighbours
                  xs'         = xs ++ neighbours'
                  v'          = foldr Set.insert v neighbours'
```

Now that I am able to move for one step, I also need to be able to compute the total gps score of a given grid.

This is simply done by getting the boxes' coordinates and applying the computational rule on these coordinates:
```hs
computeGPSScore :: Array (Int, Int) Char -> Output
computeGPSScore = sum . map (computeScore . fst) . filter ((`elem` "O[") . snd) . assocs
    where computeScore (i, j) = 100 * i + j
```

Now, I simply need to apply every move from the input to the grid, and then compute the score:

```hs
partOne :: Input -> Output
partOne = computeGPSScore . grid . uncurry (foldl move)
```

Part two is done exactly the same way. The only difference is that I need to expand the tiles first:

```hs
expandWorld :: World -> World
expandWorld world = World g' start
    where expandChar '#'  = "##"
          expandChar 'O'  = "[]"
          expandChar '.'  = ".."
          expandChar '@'  = "@."
          g               = grid world
          (height, width) = snd $ bounds g
          width'          = (width + 1) * 2
          g'              = listArray ((0, 0), (height, width' - 1)) . concatMap expandChar $ elems g
          start           = fst . head . filter ((== '@') . snd) . assocs $ g'

partTwo :: Input -> Output
partTwo = partOne . first expandWorld
```

## The end part

My solution is quite slow (about 1s). I suspect that the (//) operator is the reason for this, as it is O(n) where n is the size of the array (so the number of tiles in the grid).

In order to improve that, I see two solutions:
- Using Data.Map or Data.Set to represent the grid (this could also improve space complexity)
- Finding how diffarray work in Haskell (according to my research, the diffarray package provides an array type which has a linear complexity on the number of operations). This could be very nice to look up, as it would made for a technically purely functional approach while also being very nice to work with.
