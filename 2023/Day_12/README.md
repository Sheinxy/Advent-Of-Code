## Day 12

Dynamic programming? Nah, I'mma do my own thing 😸

```hs
import Data.List (intercalate)
import Data.Maybe (isJust, fromJust)
import Data.Map hiding (map)
import qualified Data.Map as M (lookup)
import Data.Tuple.Extra ((***))
import System.Environment

type Input = [(String, [Int])]
type Output = Int

parseInput :: String -> Input
parseInput = map (go . words) . lines
    where go [s, ns] = (s, read $ "[" ++ ns ++ "]")

-- Memoization types
type Key = (Int, String, [Int]) -- (Current block length, tiles, groups)
type Memo = Map Key Int         -- State -> Number of arrangement given by that state

-- Update the cache and return both the cache and the value
updateMemo :: Memo -> Key -> Int -> (Memo, Int)
updateMemo m k v = (insert k v m, v)

-- Compute the number of possible arrangements with a cache
computeWithMemo :: Memo -> Key -> (Memo, Int)
computeWithMemo m k@(n, s, g) | isJust r = (m, fromJust r) where r = M.lookup k m         -- Cached result

computeWithMemo m k@(n, _, c:_)  | n > c = updateMemo m k 0                               -- Invalid arrangement: current block is too big

computeWithMemo m k@(0, xs, [ ]) | all (`elem` "?.") xs           = updateMemo m k 1      -- Valid arrangement: end of groups
computeWithMemo m k@(_, xs, [ ]) | '#'  `elem`       xs           = updateMemo m k 0      -- Invalid arrangement: more blocks but no more groups
computeWithMemo m k@(n, xs, [r]) | all (`elem` "?.") xs && r == n = updateMemo m k 1      -- Valid arrangement: current block ended with the right length for the last group
computeWithMemo m k@(n, "",   _) = updateMemo m k 0                                       -- Invalid arrangement: end of strings with leftover groups

computeWithMemo m k@(0, '.' : xs, groups) = updateMemo m' k v                                   -- Case '.' while not on block: keep going
                                          where (m', v) = computeWithMemo m (0, xs, groups)
computeWithMemo m k@(n, '.' : xs, c:rest) | n == c      = updateMemo m' k v                     -- Case '.' while on block: block has the right length
                                          | otherwise   = updateMemo m k 0                      -- Case '.' while on block: block has not the right length
                                          where (m', v) = computeWithMemo m (0, xs, rest)

computeWithMemo m k@(n, '#' : xs, groups) = updateMemo m' k v                                   -- Case '#': keep going and count this tile in the block length
                                          where (m', v) = computeWithMemo m (n + 1, xs, groups)

computeWithMemo m k@(n, '?' : xs, groups) = updateMemo m'' k (v + v')                           -- Case '?': Check what happens if '.' and if '#'. Add the number of arrangement for each possibility
                                          where (m' , v ) = computeWithMemo m  (n, '.' : xs, groups) 
                                                (m'', v') = computeWithMemo m' (n, '#' : xs, groups)
partOne :: Input -> Output
partOne = sum . map (\(s, g) -> snd . computeWithMemo empty $ (0, s, g))

partTwo :: Input -> Output
partTwo = partOne . map (intercalate "?" . replicate 5 *** concat . replicate 5)
```

Don't worry, everything is quite easy.

## The input

```hs
type Input = [(String, [Int])]
type Output = Int

parseInput :: String -> Input
parseInput = map (go . words) . lines
    where go [s, ns] = (s, read $ "[" ++ ns ++ "]")
```
My input is my list of springs and group sizes. To parse it, I simply take each line from my input file.
For each line, I split by whitespaces, which gives me two parts:
 - The first part (the springs) can be taken as is.
 - The second part needs to be converted into a list of int. To do that, I used the oldest trick in the book: I had brackets around I use use the read function (😽)

## The solution

```hs
-- Memoization types
type Key = (Int, String, [Int]) -- (Current block length, tiles, groups)
type Memo = Map Key Int         -- State -> Number of arrangement given by that state

-- Update the cache and return both the cache and the value
updateMemo :: Memo -> Key -> Int -> (Memo, Int)
updateMemo m k v = (insert k v m, v)

-- Compute the number of possible arrangements with a cache
computeWithMemo :: Memo -> Key -> (Memo, Int)
computeWithMemo m k@(n, s, g) | isJust r = (m, fromJust r) where r = M.lookup k m         -- Cached result

computeWithMemo m k@(n, _, c:_)  | n > c = updateMemo m k 0                               -- Invalid arrangement: current block is too big

computeWithMemo m k@(0, xs, [ ]) | all (`elem` "?.") xs           = updateMemo m k 1      -- Valid arrangement: end of groups
computeWithMemo m k@(_, xs, [ ]) | '#'  `elem`       xs           = updateMemo m k 0      -- Invalid arrangement: more blocks but no more groups
computeWithMemo m k@(n, xs, [r]) | all (`elem` "?.") xs && r == n = updateMemo m k 1      -- Valid arrangement: current block ended with the right length for the last group
computeWithMemo m k@(n, "",   _) = updateMemo m k 0                                       -- Invalid arrangement: end of strings with leftover groups

computeWithMemo m k@(0, '.' : xs, groups) = updateMemo m' k v                                   -- Case '.' while not on block: keep going
                                          where (m', v) = computeWithMemo m (0, xs, groups)
computeWithMemo m k@(n, '.' : xs, c:rest) | n == c      = updateMemo m' k v                     -- Case '.' while on block: block has the right length
                                          | otherwise   = updateMemo m k 0                      -- Case '.' while on block: block has not the right length
                                          where (m', v) = computeWithMemo m (0, xs, rest)

computeWithMemo m k@(n, '#' : xs, groups) = updateMemo m' k v                                   -- Case '#': keep going and count this tile in the block length
                                          where (m', v) = computeWithMemo m (n + 1, xs, groups)

computeWithMemo m k@(n, '?' : xs, groups) = updateMemo m'' k (v + v')                           -- Case '?': Check what happens if '.' and if '#'. Add the number of arrangement for each possibility
                                          where (m' , v ) = computeWithMemo m  (n, '.' : xs, groups) 
                                                (m'', v') = computeWithMemo m' (n, '#' : xs, groups)
```

Alright, this is quite a big chunk of code. And you should know by now that I don't like analyzing big chunks of code. Instead, I will describe my solution in two parts:

### Part 1: the initial reasoning (aka bruteforcing)

In order to find how many arrangements can be done, I simply need to try them all! 😸
In order to do that, I scan my row and I keep track of a few things:
 - The length of the current ongoing block of damaged springs
 - The springs that need to be scanned
 - The groups that I haven't mapped to a block of damaged springs yet

The idea being:
 - If I find a state where there is either 0 or 1 possible arrangement, I can stop here for that state (no need to scan further). This is the case, for example, when there is no more group left to find while there still are damaged blocks further down the line. Or another example is when there is no more group left to find and there are no more damaged springs. If the former case, the arangement is invalid and therefore this state has 0 possible arrangements, while in the latter only one case is valid (the one where all the next ? are .)
 - When I find a . that follows a block of damaged springs, it marks the end of that block. If the block was too small, then this is an invalid arrangement. Otherwise, this may yield some valid arrangements therefore I need to keep scanning, and I map the next group to this ended block.
 - When I find a . that doesn't follow a block, I just keep scanning further.
 - When I find a ?, I consider both cases where it is a '.' and a '#'. The number of valid arrangements for the '?' is the sum of valid arrangements for the '.' case and the '#' case.

### Part 2: oh no, this is slow (aka memoization)

Sadly, this bruteforce solution has a small problem:
  - Some states might get recomputed, multiple times.

In order to work around that problem, I simply keep my results inside a Map (which I use as a cache).

This Map maps each State (which is represented by my parameters) to its result. The function starts by checking if the state is present in the map, and if it is it just returns the value from the map.

Instead of returning directly the value, my function returns the map alongside the value, as it was easier for me to code.

### Part 3 (bonus) : this is still slow

Now, now, this ran in about 10s on my slow laptop, and 7s on my Mac.

But I still want it to go faster! 😾

Well, here is the bonus round:
 - Step 1: I compiled with -O2 (yes yes, I was actually not doing that, dunno why :,D). This speeds up my code from 7s to 4s on my Mac.
 - Step 2: PARALLELISATION GOES BRRRRRRRRRRRRR
     ```hs
    import Control.Parallel.Strategies

    partOne :: Input -> Output
    partOne = sum . parMap rseq (snd . computeWithMemo empty) . map (\(s, g) -> (0, s, g))
    ```

With that two steps, my code now runs in about a second 😸 
