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

## Bonus: I'm not feeling sad anymore

Alright, I was a bit sad that my solution was just a simple "import library",
therefore I did it all by myself :D

The idea here is pretty simple:
- Merge ranges together when they can be merged
- Then just add the cardinalities

In order to merge ranges together, I start by sorting them. In that way,
I make sure that they are adjacent in the list.

Then, I use my own version of groupBy, which I called groupByNT:

```hs
-- Similar to groupBy but without the transitive property
-- Each element in the sublist matches the predicate with at
-- least one other element in the list before it.
groupByNT :: (a -> a -> Bool) -> [a] -> [[a]]
groupByNT predicate = groupByNT' []
    where groupByNT' acc [] = [reverse acc]
          groupByNT' [] (x : xs) = groupByNT' [x] xs
          groupByNT' acc l@(x : xs) | any (predicate x) acc = groupByNT' (x : acc) xs
                                    | otherwise = (reverse acc) : groupByNT' [] l
```

The thing with the regular groupBy is that the predicate is applied with the first element of the group.
Here, the intersection predicate for some range might only work with the second element of the group,
or the third, or the fourth etc.

Take for example the following ranges:
```hs
[(1, 4), (2, 10), (5, 6)]
```

With groupBy, we would get
```hs
[[(1, 4), (2, 10)], [(5, 6)]]
```

Because the first element of the group doesn't intersect with (5, 6).

We also need to check the whole group, and not just the directly adjacent element:
```hs
[(1, 4), (2, 10), (5, 6), (7, 8)]
```

With this function, I can compute the unions for the ranges:
- start by sorting them
- group them when they intersect
- the new range starts at the smallest bound and ends at the highest one

 ```hs
unionize :: [(Int, Int)] -> [(Int, Int)]
unionize = map (\l -> (minimum . map fst $ l, maximum . map snd $ l))
         . groupByNT intersects . sort
    where intersects (a, b) (c, d) = (a `between` c $ d) || (c `between` a $ b)

partTwo' :: Input -> Output
partTwo' = sum . map (\(a, b) -> b - a + 1) . unionize . fst
```

```sh
➜  Advent-Of-Code git:(main) ✗ cabal run AOC2025 05 one two two\' 2025/inputs/05
Day 05:
635
    Part one: 8.360 ms
369761800782619
    Part two: 1.547 ms
369761800782619
    Part two': 1.747 ms
Total:
    12.47 ms
```
