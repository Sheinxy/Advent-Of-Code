## Day 21

Today was exhausting.

![Exhausted cat](https://i.pinimg.com/originals/ea/67/a6/ea67a62202f703a351e20354be1d7876.jpg)

---

## The input

The input is a list of codes, each code appearing on a separate line.

I simply split the input by lines to get each code as a `String`:

```hs
parseInput :: String -> Input
parseInput = lines
```

---

## The first headache: understanding the puzzle

Today was hard for many reasons.

The first reason is that it was really difficult to understand.

The basic idea is as follows:
- We have a numpad and directional keypads (which I'll refer to as "keypad").
- A robot has to press a code on the numpad.
- Another robot controls the previous robot using a keypad.
- And yet another robot controls *that* robot using a keypad.
- Finally, *we* control the last robot using a keypad.

We need to find the length of the shortest key-press combinations that will
cascade through all the robots, ultimately making the first robot press the desired code.

Each robot starts on the key 'A'.

For example, if I press the following keys: `<A>AvA^A`,
the robot I control will press: `^A>A`.

(This example is technically invalid, but it's just to illustrate the idea.)

Essentially, we need to find the key-presses that propagate in the right sequence.

![Crazy explanations](https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Fmedia1.tenor.com%2Fimages%2F14c1bcb6e0eb7c5ff4ac225f1114e819%2Ftenor.gif%3Fitemid%3D13036231&f=1&nofb=1&ipt=102e094f179b942e3d0d1041fd88208ba8d755bafde8d4f13204eb387bf5659a&ipo=images)

---

## The second headache: propagating key-presses

Today was hard for many reasons.

The second reason was figuring out how to propagate key-presses properly.

Let’s start by solving a subproblem first:

### What keys does the second robot need to press for the first robot to enter the code?

Ultimately, this boils down to finding the shortest path between each consecutive digit
and concatenating all these paths into a single sequence.

For example, to enter '369A', the shortest paths are:
- A -> 3 = `^`
- 3 -> 6 = `^`
- 6 -> 9 = `^`
- 9 -> A = `vvv`

So the second robot will need to press `"ˆ^A^A^AvvvA"`.

Now, some digits may have multiple valid paths between them.

In such cases, we can prune paths based on the following rule:
- We only care about paths that can be split into two parts: vertical movements and horizontal movements.
  This is because we want to minimize movement, enabling the next robot to press 'A' multiple times in succession.

For example, to enter the code "343A":
- A -> 3 = [`^`]
- 3 -> 4 = [`<^`, `^<<`]
- 4 -> 3 = [`>>v`, `v>>`]
- 3 -> A = [`v`]

We can generate all possible paths using Haskell's [sequence function](https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html#v:sequence),
but we'll see that later.

This generates a list of sublists, where each sublist represents the possibilities for one step.

Let’s assume we have a `getPaths` function that generates all valid paths between two keys.

We can generate paths as follows:

```hs
findPresses :: ((Char, Char) -> [String]) -> String -> [[String]]
findPresses getPaths s = map (map (++ "A")) $ presses
    where s'      = 'A' : s
          moves   = zip s' s
          presses = map getPaths moves
```

Here, I start by adding the starting point to the path we want to create.

Then, I zip this new string with the old one to get each pair of "source -> destination".

Finally, I simply add the ending "A" to each step.

---

### How to find the shortest paths between two nodes

The numpad (and the keypad) can be represented as a grid by mapping each digit to coordinates:

```hs
numpad :: Map Char (Int, Int)
numpad = Map.fromList indexed
    where pad     = ["#0A", "123", "456", "789"]
          indexed = concat [zipWith (\c j -> (c, (i, j))) s [0 .. ] | (s, i) <- zip pad [0 .. ]]

keypad :: Map Char (Int, Int)
keypad = Map.fromList indexed
    where pad     = ["#^A", "<v>"]
          indexed = concat [zipWith (\c j -> (c, (i, j))) s [0 .. ] | (s, i) <- zip pad [0 .. ]]
```

To compute paths between two nodes, we calculate horizontal and vertical movements:

The path is either going to be:
- vertical movements -> horizontal movements
- horizontal movement -> vertical movements

If we want to be even safer, we can try all the possible permutations of doing x horizontal movements and y vertical ones.

One thing to handle is to check that the movements don't lead to the empty square (which i've represented with (0, 0)):

```hs
computePaths :: Map Char (Int, Int) -> Char -> Char -> [String]
computePaths m src dst | i == 0 = filter (not . leadsToVoidH) paths
                       | j == 0 = filter (not . leadsToVoidV) paths
                       | otherwise = paths
    where (i, j) = m Map.! src
          (k, l) = m Map.! dst

          dx = l - j
          dy = k - i

          horDir | dx < 0    = '<'
                 | otherwise = '>'
          verDir | dy < 0    = 'v'
                 | otherwise = '^'

          moves = replicate (abs dy) verDir ++ replicate (abs dx) horDir
          paths = [moves, reverse moves] -- permutations moves
          leadsToVoidH = ((replicate j '<') `isPrefixOf`)
          leadsToVoidV = ((replicate i 'v') `isPrefixOf`)
```

Now we can simply compute all the paths:
```hs
allNumpadPaths :: Map (Char, Char) [String]
allNumpadPaths = Map.fromList [((s, d), computePaths numpad s d) | s <- pad, d <- pad]
    where pad = filter (/= '#') $ Map.keys numpad

allKeypadPaths :: Map (Char, Char) [String]
allKeypadPaths = Map.fromList [((s, d), computePaths' s d) | s <- pad, d <- pad]
    where pad = filter (/= '#') $ Map.keys keypad
          computePaths' s d = map (map flipDir) $ computePaths keypad  s d
          flipDir '^' = 'v'
          flipDir 'v' = '^'
          flipDir c   = c

getNumpadPaths :: (Char, Char) -> [String]
getNumpadPaths = (allNumpadPaths Map.!)

getKeypadPaths :: (Char, Char) -> [String]
getKeypadPaths = (allKeypadPaths Map.!)
```

---

Note that I am somewhat cheating here:

I reverse the lines of the keypad in order to make the code simpler.

This means that I have to flip the vertical movements after computing the path.

---

Now getting the paths for a given string is easy:
```hs
findNumPresses :: String -> [[String]]
findNumPresses = findPresses getNumpadPaths

findDirPresses :: String -> [[String]]
findDirPresses = findPresses getKeypadPaths
```

---

## The third headache: propagating sequences

Today was hard for many reasons.

The third reason was propagating paths effectively.

The core idea is simple:
- A path consists of smaller steps, all starting and ending at 'A'.
- We need to find the shortest sequence of presses to execute those steps.
- Then, we select the path resulting in the fewest presses.

For each step:
- If we’re on the last keypad, execute the step directly.
- Otherwise, compute the sequence recursively, memoizing results.

```hs

-- The idea here is actually pretty simple:
-- - A path is actually divided into smaller steps. All these steps start and end on A.
-- - So all we need to do is to find the shortest sequence of presses that will give that step.
-- - We can then keep the path that uses the steps leading to the smallest number of presses.

-- To find the number of presses a step will lead to:
-- - If we're on the last keypad, then this step will be performed, therefore the number of presses is its length
-- - Otherwise, we find the paths generating that step. We do that by getting the paths for each step and calling sequence on the result.
--     - For each path, we find the smallest number of presses for each step, and we sum the total length of the path.
-- - We take the path leading to the smallest amount of presses.

findSequence :: (Int -> String -> Int) -> Int -> String -> Int
findSequence f 0 s = length s
findSequence f n s = getRes s
    where findSequence' = f (n - 1)
          getRes  = minimum .
                    map (sum . map findSequence') .
                    sequence .
                    findDirPresses
```

My initial implementation returned the string itself instead of just the length, which lead to some longggg computations (probably because it's not that easy to memoize.)

---

To be frank, I don't exactly know how sequence works and why this is the result it gives me,
I'll need to look deeper about it (after 4 years it may be time for me to learn what a monad is?)

---


---

## Wrapping it up

We can now compute the result for any number of intermediate robots:

```hs
solveWith :: Int -> String -> Int
solveWith robots = minimum .
                   map (sum . map (findSequenceMem robots)) .
                   sequence .
                   findNumPresses
    where findSequenceMem = memoFix2 findSequence
```

Part 1 and 2:

```hs
partOne :: Input -> Output
partOne = sum . map computeComplexity
    where computeComplexity code = (read . init) code * solveWith 2 code

partTwo :: Input -> Output
partTwo = sum . map computeComplexity
    where computeComplexity code = (read . init) code * solveWith 25 code
```

---

## The last headache: writing the writeup

I’ll be honest—writing this explanation at 4 AM after struggling with the solution was tough.

I'd need to create some animations of how my solution works for my explanations to be easy to understand, but it's too late for me right now.
I enjoyed the challenge, but puzzles like this can be exhausting when you need an entire evening to solve them. 😞
