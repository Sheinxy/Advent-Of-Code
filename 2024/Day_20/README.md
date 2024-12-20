## Day 20

Today was surprisingly not that hard.

I had a very good idea for part 1 which worked immediately,
then I struggled adapting it for part 2.

I took a long break, went back to it and realised that adapting it,
just meant changing a constant to a variable.

## The input

[Day 16.](../Day_16)

```hs
data World = World { grid :: Array (Int, Int) Char, start :: (Int, Int), end :: (Int, Int) } deriving Show

parseInput :: String -> Input
parseInput input = World grid start end
    where gridLines = lines input
          (height, width) = (length gridLines, length $ head gridLines)
          grid  = listArray ((1, 1), (height, width)) $ filter (/= '\n') input
          start = fst . head . filter ((== 'S') . snd) . assocs $ grid
          end   = fst . head . filter ((== 'E') . snd) . assocs $ grid
```

## Part 1

### The problem

For a given number of steps (picoseconds), we want to find the number
of shortcuts that can save this number of steps.

### The solution

#### Finding the path

Let's first start by finding the actual path for the racetrack.

This can be simply done by launching a bfs (or a dfs, both work here).

There is only one single path from start to end, so this bfs just allows us
to get the order of each tile in the path.

```hs
getRaceTrack :: Input -> [(Int, Int)]
getRaceTrack (World g s e) = fromJust $ bfs getNeighbours (== e) s
    where getNeighbours (i, j) = filter ((/= '#') . (g !)) [(i + di, j + dj) | di <- [-1 .. 1],
                                                                               dj <- [-1 .. 1],
                                                                               abs di + abs dj == 1]
```

#### Finding the number of shortcuts for X

Let's picture the following path:

```
123
0#4
```

Let's say that we want to save 2 steps. The only way here is to get the following
path:
```
...
012
```

Another example, where we now want to save four steps (due to the way cheating works,
it is only possible to skip even numbers of steps I believe):

```
#####
#345#
#2#6#
#1#7#
#0#8#
```

Here we can skip like that:
```
#####
#...#
#.#.#
#123#
#0#4#
```

Generalising this, to skip n steps:
- We need to connect what was step x with step x + n + 2
- If step x and step x + n + 2 are separated by a distance of 2

This distance of 2 comes from the number of steps it takes to traverse the wall.

This generalisation can even be made further:
- If we can traverse up to m tiles without caring about collisions,
    then we need to check that steps x + n + m are separated by a distance of m.

```hs
getCheats :: [(Int, Int)] -> Int -> Int -> [((Int, Int), (Int, Int))]
getCheats path cheat_dist save = cheats
    where associations = zip path (drop (save + cheat_dist) path)
          cheats       = filter ((== cheat_dist) . uncurry taxicab) associations
```

The way this works is by zipping each step of the path with the one that comes after n + m picoseconds.
Then it filters the pairs that are indeed at a distance of m from each other.

Now we simply need to launch that for 100 picoseconds, 102, etc. all the way
up to the total length of the path (minus 2 because we want to be efficient,
zipping the path with an empty list is not useful)

```hs
partOne :: Input -> Output
partOne w = length cheats
    where path   = start w : getRaceTrack w
          len    = length path
          cheats = concat $ parMap rseq (getCheats path 2) [100, 102 .. len - 2]
```

## Part 2

### The problem

We can now walk up to 20 tiles without caring about collisions.

### The solution

#### The generalisation

Let's look at this example to see how my generalisation works:
```
#############
#345#DEF#NOP#
#2#6#C#G#M#Q#
#1#7#B#H#L#R#
#0#89A#IJK#S#
#############
````

Here, I can save 17 steps by cheating during 10 picoseconds:
```
#############
#...#...#...#
#.#.#.#.#.#.#
#123456789AB#
#0#...#...#C#
#############
````

To find that shortcut:
- We can pair step 1 and step R together, as they are 17 + 10 steps apart
- The distance between step 1 and step R is exactly 10

Thereforce there is a shortcut between them.

#### The bruteforce

- For each possible number of cheat curation
- We try to find the number of cheats that save x steps

```hs
partTwo :: Input -> Output
partTwo w = length cheats
    where path   = start w : getRaceTrack w
          len    = length path
          cheats = concatMap (getCheats' path) [2 .. 20]
          getCheats' path dist = concat $ parMap rseq (getCheats path dist) [100, 102 .. len - dist]
```

## Total time

Note here that I am multithreading to go a little bit faster:
```
➜  Day_20 git:(main) ✗ time ./Day_20 one two input
1343
982891
./Day_20 one two input  6.79s user 0.13s system 381% cpu 1.811 total
```

1.8s! That's quite fine!

## The end part

I'm really surprised at how fast I found my solution for part 1.
