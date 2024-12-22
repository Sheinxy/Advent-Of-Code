## Day 22

My solution is slow, but it works.

![it ain't much but it's honest work](https://i.imgflip.com/3gzjfj.jpg)

---

## The input

The input is pretty simple:
- Each line represents a single number.

```hs
type Input = [Int]

parseInput :: String -> Input
parseInput = map read . lines
```

---

## Part 1

### The problem

We need to compute the 2000th secret number for each number in the input.

---

### The solution

To compute the secret number for a given input:
- Multiplying by 64 is equivalent to shifting left by 6.
- Dividing by 32 is equivalent to shifting right by 5.
- Multiplying by 2048 is equivalent to shifting left by 11.
- Taking a number modulo 16777216 is equivalent to keeping only the last 24 bits.

```hs
computeNextSecret :: Int -> Int
computeNextSecret x = res
    where y   = ((x `shift`   6)  `xor` x) .&. 0x00FFFFFF
          z   = ((y `shift` (-5)) `xor` y) .&. 0x00FFFFFF
          res = ((z `shift`  11)  `xor` z) .&. 0x00FFFFFF
```

Next, we repeat this operation 2000 times:

```hs
getSecrets :: [Int] -> [[Int]]
getSecrets = map (take 2001 . iterate computeNextSecret)
```

Finally, we sum up the 2000th number for each input line:

```hs
partOne :: Input -> Output
partOne = sum . map last . getSecrets
```

---

## Part 2

### The problem

We need to find the sequence of 4 differences that yields the highest sum.

---

### The solution

We start by computing the number of bananas a given sequence of differences produces for each list of secrets.

1. First, extract the last digit of each number.
2. Compute the differences between consecutive numbers.
3. Group these differences into chunks of 4.
4. Map each chunk into a pair:
   - The sequence of differences.
   - The last digit (representing the number of bananas).

```hs
chunksOf :: Int -> [a] -> [[a]]
chunksOf n = takeWhile ((n ==) . length) . map (take n) . tails

computeSequences :: [Int] -> Map [Int] Int
computeSequences secrets = Map.fromListWith (\_ x -> x) . map arrange $ chunks
    where zipDiff a b = (a, b - a)
          arrange l   = (map snd l, (fst . last) l)
          digits  = map (`mod` 10) secrets
          diffs   = zipWith zipDiff (tail digits) digits
          chunks  = chunksOf 4 diffs
```

Now, we compute the above mappings for each input number.

Next, we gather all possible sequences and bruteforce the maximum number of bananas.

We also add some parallelization for performance:

```hs
partTwo :: Input -> Output
partTwo input = (Set.findMax . Set.map bananas) sequences
    where bananas sequence = sum . parMap rseq (Map.findWithDefault 0 sequence) $ mappings
          secrets   = getSecrets input
          mappings  = parMap rseq computeSequences secrets
          sequences = Set.fromList (concatMap Map.keys mappings)
```

---

## The end part

Today wasn’t too hard, but there are still many optimizations I haven’t explored or implemented yet.
