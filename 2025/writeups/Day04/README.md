## Day 04

I feel like I say this every year, but I'll say it again:
puzzles involving 2D grids in Haskell intimidate me.

I was this close to using IOArrays but after thinking about it it was stupid.

## The Input

The input is a 2D grid. The important part of that grid is that there are
'@' representing paper rolls on it. These are all we care about,
which is why my input is just the set of their positions.

I get that set by indexing the grid, filtering out the empty tiles, and only
keeping the indices:

```hs
type Input = S.Set (Int, Int)

parseInput :: String -> Input
parseInput raw = S.fromList . map fst . filter ((== '@') . snd) $
            [ ((i, j), x)
            | (i, row) <- zip [0..] grid,
              (j, x) <- zip [0..] row
            ]
    where grid = lines raw
```

## Part One

### The Problem

We want to find the number of rolls that are surrounded by less than 4 other rolls.

### The Solution

We find the number of rolls that are surrounded by less than 4 other rolls.

No, really, that's it.

I simply filter these rolls, and my filtering function just checks whether the neighbouring tiles
are members of the set and that there are less than 4 of them.

```hs
type Output = Int

findAccessible :: Input -> Input
findAccessible rolls = S.filter isAccessible rolls
    where isAccessible (i, j) = length adjacents < 4
            where neighbours = [(i + di, j + dj) | di <- [-1 .. 1], dj <- [-1 .. 1], di /= 0 || dj /= 0]
                  adjacents = filter (`S.member` rolls) neighbours

partOne :: Input -> Output
partOne = S.size . findAccessible
```

## Part Two: say that again?

### The Problem

Now we need to remove the accessible rolls and do it until we cannot access any more rolls. How many have we removed?

### The Solution

We remove the accessible rolls and dot it until we cannot access any more rolls. We count how many we have removed.

This is just a question of iterating until a fixed point. Basically, recurse until you get the
same result.

```hs
partTwo :: Input -> Output
partTwo rolls = S.size rolls - S.size (fix removeRolls rolls)
    where removeRolls rolls' = rolls' \\ findAccessible rolls'
          fix f x = let x' = f x in if x == x' then x else fix f x'
```

⚠️ After thinking about it more, I realized that checking if findAccessible yields
and empty set might be slightly faster:

```hs
partTwo :: Input -> Output
partTwo rolls = S.size rolls - S.size finalState
    where removeRolls rolls' = rolls' \\ findAccessible rolls'
          finalState = until (S.null . findAccessible) removeRolls rolls
```

## Conclusion

I don't really know what to write here lol.
