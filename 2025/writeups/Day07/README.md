## Day 07

Dynamic programming? Pruning? Only computing what’s necessary?

Nah, I’mma just memoise :)

## The Input

The input is simply a 2D grid with three types of tiles:

- `.` — an empty tile
- `S` — the starting position
- `^` — a splitter tile

We only actually care about the last two, so my input is a tuple containing the starting position and the positions of all splitter tiles:

```hs
type Input = ((Int, Int), [(Int, Int)])
```

To get them, I start by indexing the grid using a little utility function:

```hs
-- Takes a 2D list and returns an indexed 1D list.
index2D :: [[a]] -> [((Int, Int), a)]
index2D g = [((i, j), x) | (i, row) <- zip [0..] g, (j, x) <- zip [0..] row]
```

Parsing the input is straightforward:

- split into lines,
- index the grid,
- find the element containing `S`,
- filter all elements containing `^`.

```hs
parseInput :: String -> Input
parseInput = (fst . findJust ((== 'S') . snd) &&& map fst . filter ((== '^') . snd))
           . index2D . lines
```

## Part One

### The Problem

A beam starts by going down from the starting position. When it hits a splitter, it splits into two beams (one going left, one going right), then continues downward. When two beams arrive at the same position, they merge into a single beam.

How many times does the beam split?

Or phrased differently: how many splitter tiles do the beams touch?

### The Solution

Let’s start naïvely and work up from there.

First question: given a starting position, which splitter (if any) will the beam encounter?

This is simple: it’s just the first splitter below it in the same column.

Because my list is ordered (thanks to how I parsed the input), I can use [`find`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:find) to locate the first valid candidate:

```hs
findStop :: (Int, Int) -> [(Int, Int)] -> Maybe (Int, Int)
findStop (r, c) = find (\(i, j) -> r <= i && j == c)
```

Once we find a splitter, what do we do?

Pretty simple: create two beams and recurse.

```hs
partOne :: Input -> Output
partOne (start, splitters) = length $ go start
    where go s = case findStop s splitters of
                    Nothing     -> []
                    Just (i, j) -> (i, j) : (go (i, j - 1) ++ go (i, j + 1))
```

Does it work?

No :)

```
➜  Advent-Of-Code git:(main) ✗ cabal run AOC2025 07 toomanycooks 2025/inputs/07.sample
Day 07:
39
    Part toomanycooks: 252.2 μs
Total:
    1.070 ms
```

The issue is pretty easy to grasp. Consider the following example (taken from the puzzle description):

```
.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
```

Multiple beams can hit the same splitters. The simple fix is to `nub` the result, or store it in a `Set`.

```hs
partOne :: Input -> Output
partOne (start, splitters) = length (go start)
    where go s = case findStop s splitters of
                    Nothing     -> []
                    Just (i, j) -> S.insert (i, j) $ S.union (go (i, j - 1)) (go (i, j + 1))
```

However, we’ve got another issue: this is slow because we're recomputing the same things over and over.
We *could* optimise by not recomputing already computed things, but honestly: memoizing everything is much easier :)

To do that, I’m using [`memoFix`](https://hackage.haskell.org/package/memoize-1.1.2/docs/Data-Function-Memoize.html#v:memoFix). (Maybe one day I'll write a blog article explaining how it works, because I love this function).

```hs
partOne :: Input -> Output
partOne (start, splitters) = S.size . memoFix go $ start
    where go f s = case findStop s splitters of
                        Nothing     -> S.empty
                        Just (i, j) -> S.insert (i, j) $ S.union (f (i, j - 1)) (f (i, j + 1))
```

Sure, it’s not the *fastest* possible solution. We could optimise in a few ways:

- prune search paths since beams only go downward,
- avoid recomputing beams we later `nub` anyway.

But honestly, I don’t care. It’s simple, elegant, and fast enough:

```
➜  Advent-Of-Code git:(main) ✗ cabal run AOC2025 07 one 2025/inputs/07   
Day 07:
1711
    Part one: 40.09 ms
Total:
    40.78 ms
```

## Part Two

### The Problem

Now we need to count the number of unique paths a beam can take.

### The Solution

Since the beam always travels downward, there are only two possibilities when it starts:

- it never encounters a splitter -> exactly 1 path
- it hits a splitter -> the total paths equal the sum of the paths from both new beams

```hs
partTwo :: Input -> Output
partTwo (start, splitters) = go start
    where go s = case findStop s splitters of
                        Nothing     -> 1
                        Just (i, j) -> go (i, j - 1) + go (i, j + 1)
```

But once again, we’re recomputing the same things a lot.

The easiest solution is once again to cache the result in order to not compute them again. Let's memoize!

```hs
partTwo :: Input -> Output
partTwo (start, splitters) = memoFix go start
    where go f s = case findStop s splitters of
                        Nothing     -> 1
                        Just (i, j) -> f (i, j - 1) + f (i, j + 1)
```

Still very fast :D

```
➜  Advent-Of-Code git:(main) ✗ cabal run AOC2025 07
Day 07:
1711
    Part one: 41.60 ms
36706966158365
    Part two: 11.67 ms
Total:
    54.23 ms
```

## Conclusion

Memoization goes brrrrrrrrrr :3
