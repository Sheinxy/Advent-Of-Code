## Day 19

Today is [memoization](https://en.wikipedia.org/wiki/Memoization) day!

---

## The Input

The input consists of two parts:
- A comma-separated list of strings
- A list of strings where each line represents an element of the list

```hs
type Input = ([String], [String])

parseInput :: String -> Input
parseInput input = (towels, designs)
    where
        p : _ : designs = lines input
        towels = sortBy (flip compare `on` length) $ splitOn ", " p
```

I chose to sort the towels by length to attempt matching the longest prefix first later.

---

## Part 1

### The Problem

We need to determine if a word can be constructed using the given bricks.

### The Solution

This can be done quite simply:
1. Identify which bricks are prefixes of the word.
2. For each brick, remove it from the word and check if the resulting word can also be constructed using the bricks.
3. An empty word is always constructible.

```hs
isPattern :: [String] -> String -> Bool
isPattern towels "" = True
isPattern towels s  = any (isPattern towels . flip drop s . length) $ filter (`isPrefixOf` s) towels
```

Seems straightforward, right?

```hs
partNope :: Input -> Output
partNope (towels, designs) = length . filter (isPattern towels) $ designs
```

Testing it:

```shell
➜  Day_19 git:(main) ✗ time ./Day_19 nope sample
6
./Day_19 nope sample  0.01s user 0.01s system 52% cpu 0.032 total
➜  Day_19 git:(main) ✗ time ./Day_19 nope input
^C
./Day_19 nope input  99.19s user 0.92s system 98% cpu 1:41.47 total
```

OH NO!

### The Problem

Computing one design takes a long time, and we have a lot of designs to process. Worse, many computations are repeated!

### The Solution

This is a perfect use case for memoization!

Typically, I would use a [Data.Map](https://hackage.haskell.org/package/containers-0.7/docs/Data-Map-Strict.html) to store intermediate results during each recursive call. However, I decided to try something different today.

After exploring the [Haskell Wiki](https://wiki.haskell.org/index.php?title=Memoization#Memoizing_fix_point_operator), I discovered the [Data.Function.Memoize](https://hackage.haskell.org/package/memoize-1.1.2/docs/Data-Function-Memoize.html) library, which implements the fix-point operator memoization technique described in the wiki.

The core idea is to modify the `isPattern` function so that it accepts the recursive function as an argument. This allows the fix-point memoization to leverage the memoized version for recursive calls.

```hs
isPatternRec :: [String] -> (String -> Bool) -> String -> Bool
isPatternRec towels f "" = True
isPatternRec towels f s  = any (f . flip drop s . length) $ filter (`isPrefixOf` s) towels
```

Next, we create a memoized version of this function and use it:

```hs
partOne :: Input -> Output
partOne (towels, designs) = length $ filter isPatternMem designs
    where
        isPatternMem = memoFix (isPatternRec towels)
```

While this type of memoization is not the fastest (a `Set` or `Map` would likely be more efficient), I find it quite elegant.

One thing that make it slower than using a `Map`, I believe, is that the memoization
only works for one to isPatternMem. Using a `Map` it would be easier to keep
the memoization cache updated between successive calls.
I am not quite sure that this is actually the case though, as I am not yet familiar
with how fixpoint-memoization works. I'll try to improve my understanding on it,
as well as my solution.


```shell
➜  Day_19 git:(main) ✗ ghc -package memoize -O2 Day_19.hs
Loaded package environment from /Users/sheinxy/.ghc/x86_64-darwin-8.8.4/environments/default
[1 of 1] Compiling Main             ( Day_19.hs, Day_19.o )
Linking Day_19 ...
ld: warning: ignoring duplicate libraries: '-lm'
➜  Day_19 git:(main) ✗ time ./Day_19 one input
251
./Day_19 one input  1.41s user 0.29s system 79% cpu 2.133 total
```

---

## Part 2

### The Problem

This time, we don’t just need to determine whether a design is buildable; we also need to calculate the number of ways it can be constructed.

### The Solution

The approach is straightforward:
1. An empty design can be constructed in exactly one way.
2. For a non-empty design, the number of ways it can be constructed is the sum of the ways each of its sub-designs can be constructed.

For example:

`brwrr` can be constructed in two ways:
- `b, r, wr, r`
- `br, wr, r`

This process can be represented as:

- `brwrr -> sum [1, 1] = 2`
    - `rwrr -> sum [1] = 1`
        - `wrr -> sum [1] = 1`
            - `r -> sum [1] = 1`
                - (empty pattern) -> 1
    - `wrr -> sum [1] = 1`
        - `r -> sum [1] = 1`
            - (empty pattern) -> 1

```hs
numPattern :: [String] -> String -> Int
numPattern towels "" = 1
numPattern towels s  = sum . map (numPattern towels . flip drop s . length) $ filter (`isPrefixOf` s) towels
```

However, this approach has the same issue as Part 1: it recomputes values unnecessarily. So, we apply the same memoization strategy:

```hs
numPatternRec :: [String] -> (String -> Int) -> String -> Int
numPatternRec towels f "" = 1
numPatternRec towels f s  = sum . map (f . flip drop s . length) $ filter (`isPrefixOf` s) towels

partTwo :: Input -> Output
partTwo (towels, designs) = sum . map numPatternMem $ designs
    where
        numPatternMem = memoFix (numPatternRec towels)
```

---

## The end

I really need to develop a generic memoization function using `Map`, but I’m still unsure how to implement it effectively.
