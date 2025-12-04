## Day 02

My solution is silly :d

## Parsing

We have a list of ranges separated by commas.

Each range is of the form `NUMBER-NUMBER`.

Parsing is simply done by splitting on commas, then on '-' and converting to ints:

```hs
type Input = [(Integer, Integer)]

parseInput :: String -> Input
parseInput = map (last2 . map read . splitOn "-") . splitOn ","
```

## Part 1: The silly awakens

### The Problem

We need to find inside each range the values that are a repeating string.
(e.g 11, 22, 1212, 123123)

### The Solution

There are many ways to do this:

- The basic way is to go through all the values in each range, convert to a string,
 split in the middle and check if the two parts are equal.
- A smarter way is finding out that these numbers are multiples of (10 ^ n + 1),
 where n is the number of digits to be repeated, and then finding these multiples directly
 from the ranges.

I decided to go for a sillier approach (because I wanted to have fun):
- Generate all the invalid IDs and just filtering them out :3

This is pretty simple, as I said it's just the multiples of 10^n + 1:

```hs
-- https://oeis.org/A004216
-- a(n) = floor(log_10(n))
-- That's just getting the number of digits in a number
a004216 :: Integer -> Integer
a004216 n = if n <= 9 then 0 else 1 + a004216 (n `div` 10)

-- https://oeis.org/A020338
-- a(n) = n*10^(A004216(n)+1) + n
-- This is just repeating n twice :D
a020338 :: Integer -> Integer
a020338 n = n * 10 ^ (a004216 n + 1) + n

partOne :: Input -> Output
partOne input = sum . filter isInRange . map a020338 $ [1 .. 99999]
    where isInRange n = any (\(a, b) -> a <= n && n <= b) input
```

How do I know I can stop at 9999999999? I eyeballed it :3 (this could actually be done
in code to have a more robust solution, but I didn't bother, it was 1AM)

Note here that I'm assuming that the ranges don't overlap, however using map and count instead of filter and any should do the trick I believe.

## Part 2: The rise of the silly

### The Problem

Actually, digits don't just repeat once, they may repeat multiple times (oh no...)

### The Solution

So, once again the easy solution would be to just go through each value,
convert to string, and split every 2 digits, check, every 3 digits, check etc.

This might work and may be fast, I don't know. I went for the silly way.

See, when I saw that problem, I wondered:
- Is this a known sequence? Do these numbers have a name?

Yes they do, they're part of the [a239019 sequence](https://oeis.org/A239019)

And let it be known that there is code to generate them!

```
F:= proc(d) local p, R, q;
  R:= {seq(x*(10^d-1)/9, x=1..9)};
  for p in numtheory:-factorset(d) minus {d} do
    q:= d/p;
    R:= R union {seq(x*(10^d-1)/(10^q-1), x=10^(q-1)..10^q-1)};
  od:
  sort(convert(R, list))
end proc:
```

This generates all the numbers with repeating digits of size d.

I'll be honest, I'm going to do something I don't like doing here: 
I'm going to use this without understanding it. Sorry :C

Anyway, translated into Haskell:

```hs
-- https://oeis.org/A239019
-- Numbers which are not primitive words over the alphabet {0,...,9} (when written in base 10). 
-- d is the length of the numbers
a239019  :: Integer -> [Integer]
a239019 d = S.toList r'
  where r = S.fromList [x * (10 ^ d - 1) `div` 9 | x <- [1 .. 9]]
        r' = foldl step r . filter (/= d) $ primeFactors d
        step acc p = S.union acc (S.fromList [x * (10 ^ d - 1) `div` (10 ^ q - 1) | x <- [10 ^ (q - 1) .. 10 ^ q - 1]])
            where q = d `div` p

partTwo :: Input -> Output
partTwo input = sum . concatMap (filter isInRange . a239019) $ [2 .. 10]
    where isInRange n = any (\(a, b) -> a <= n && n <= b) input
```

I don't know how fast the naive way of doing it is, but this is more than acceptable:

```
➜  Advent-Of-Code git:(main) ✗ cabal run AOC2025 02 two d
Day 02:
11323661261
    Part two: 39.55 ms
```

Note once again that I stop at 10 digits numbers by eyeballing it, but this could be done in code.

## Conclusion

Never let me code at 1AM, I'm a silly cat.
