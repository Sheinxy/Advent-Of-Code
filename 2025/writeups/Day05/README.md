## Day 05

Please forgive me, as I've chosen the easy way: I used a library that does
all the heavy lifting for me...

I didn't want to think about how to compute the union efficiently (plus I'm
pretty sure I've already had to do something similar before)

## The Input

This is pretty simple, pretty standard 2-part input separated by an empty line.

In order to parse it, I first get every line, I split on the empty line.

The second part just needs conversions to Ints, while the first part is parsed
similarly to this year's day 2: simply split on the "-", convert to Ints and make that a tuple.

```hs
type Input = ([(Int, Int)], [Int])

parseInput :: String -> Input
parseInput = (\[ranges, numbers] -> (map getRange ranges, map read numbers)) . splitOn [""] . lines
    where getRange = last2 . map read . splitOn "-"
```

## Part One

### The Problem

We need to find how many numbers are in the ranges.

### The Solution

We find how many numbers are in the ranges.

It's just a [count](https://hackage-content.haskell.org/package/ghc-9.12.2/docs/GHC-Utils-Misc.html#v:count) . If any range includes the number we count it. I use a utility function that I made called `isInRange`:

```hs
-- Is a in a closed range
isInRange :: Ord a => a -> (a, a) -> Bool
isInRange a (b, c)= between a b c
```

This is part of a small library of things I often do in AOC uwu.

```hs
partOne :: Input -> Output
partOne (ranges, numbers) = count (\x -> any (isInRange x) ranges) numbers
```

## Part Two: I got lazy

### The Problem

We actually need to find how many numbers there are in total when computing the union of all the ranges.

### The Solution

I had a choice here:
- I could have written a "unionize" function that would compute it.
- Or I could use a library that is suited for manipulating ranges and that already does it.


The second solution isn't really satisfying because it just requires a small search on [hoogle](https://hoogle.haskell.org/?hoogle=%5B(Int%2C%20Int)%5D%20-%3E%20%5B(Int%2C%20Int)%5D) to see if something like this exists...


Anyways, let me introduce you to [Data.RangeSet.List](https://hackage.haskell.org/package/range-set-list-0.1.4/docs/Data-RangeSet-List.html), my new favorite guy :)


```hs
partTwo :: Input -> Output
partTwo = RSet.size . RSet.fromRangeList . fst
```

## Conclusion

I'm sorry, but I like my small numbers:

```
➜  Advent-Of-Code git:(main) ✗ cabal run AOC2025 05 one two 2025/inputs/05
Day 05:
635
    Part one: 8.465 ms
369761800782619
    Part two: 1.506 ms
Total:
    10.79 ms
```


