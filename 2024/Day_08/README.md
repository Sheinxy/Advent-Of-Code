## Day 08

![Let's do math cat](https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Ftse1.mm.bing.net%2Fth%3Fid%3DOIP.9ENKTUgHNsG3fXqQTUiiAwHaHa%26pid%3DApi&f=1&ipt=1b07726fb8a08ee114b101c43dec43198c24878def6f07a8c126721c0bd0bae2&ipo=images)

## The input

Yet again another grid to parse, let's do it differently once again for fun!

First of all, let's look at the most important information to retrieve:
- The antennas and their types
- The dimensions of the grid

Let's compile that in a data type:
```hs
data World = World { antennas :: [[(Int, Int)]], height :: Int, width :: Int } deriving (Show)
```

Here, I've decided to put the antennas as a list of list of coordinates.

Each sublist represents the coordinates of each antenna of one type.

For example:

```
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
```

Antennas here is:
```hs
[[(2,9),(3,6),(4,8),(5,5)],[(6,7),(9,9),(10,10)]]
-- Coordinates start at 1 because I felt like it.
```

We don't really need to keep the character representing the antenna's frequency.

In order to get the antennas, I start by indexing each tile of the grid
using a zip:
```hs
zip
[(i, j) | i <- [1 .. height], j <- [1 .. width]] $
filter (/= '\n') input
```

Then, I filter all the non '.' characters:
```hs
filter ((/= '.') . snd) $
zip
[(i, j) | i <- [1 .. height], j <- [1 .. width]] $
filter (/= '\n') input
```

I sort the result according to the character, and I group the antennas together.

I then dispose of the character as it isn't of any use to us.

```hs
parseInput :: String -> Input
parseInput input =  World antennas height width
    where grid = lines input
          height = length grid
          width  = length $ head grid
          antennas = map (map fst)           $ 
                     groupBy ((==) `on` snd) $ 
                     sortOn snd              $
                     filter ((/= '.') . snd) $
                     zip
                     [(i, j) | i <- [1 .. height], j <- [1 .. width]] $
                     filter (/= '\n') input
```

## Part one and two

Yes, I am going to group the solutions together, because they're basically the same.

### The problem

Each pair of antennas of the same type forms a line. We need to trace that line.

For part 1, we only need to trace the first step forwards and the first step backwards.

For part 2, we need to trace all steps.

### The solution

We simply need to define our line using [parameteric equations](https://en.wikipedia.org/wiki/Line_(geometry)#Parametric_equation)

Let's say that we have two points (our two antennas) forming a line: (x1, y1) and (x2, y2),

Then the parametric equation of that line is (x1 + at, y1 + bt), where (a, b) represents the slope, given by (x2 - x1, y2 - y1).

We can therefore find that slope by computing the difference between each coordinats of our points. Then we can trace them forward by adding that difference to one point for each step (t) that we need, and trace them backward by subtracting.


```hs
getAntinodes :: [Int] -> (Int, Int) -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
getAntinodes steps (height, width) (i1, j1) (i2, j2) = antinodes
    where (di, dj)   = (i1 - i2, j1 - j2)
          an1        = takeWhile isInGrid [(i1 + t * di, j1 + t * dj) | t <- steps]
          an2        = takeWhile isInGrid [(i2 - t * di, j2 - t * dj) | t <- steps]
          antinodes  = an1 ++ an2
          isInGrid (i, j) = 1 <= i && i <= height && 1 <= j && j <= width
```

Note a few things here:

- I use (i1 - i2, j1 - j2) as my slope here. It doesn't really matter considering that the parametric equation holds for any t (therefore t can be negative).
- I move forwards starting from (i1, j1) and backwards starting from (i2, j2), but I could've started from the same point: the parametric equation will still hold not matter what (x1, y1) I choose.

Once I have that, all I need to do is to get the antinodes for each pair of antennas of each antenna group. For part 1 I only need to use t = 1, while for part 2 I
need to use every possible t starting from 0:

```hs
partOne :: Input -> Output
partOne world = length . nub . concat $
                [getAntinodes [1] dims a1 a2 | antennaGroup <- antennas world,
                                               a1 <- antennaGroup,
                                               a2 <- antennaGroup,
                                               a1 < a2]
    where dims = (height world, width world)

partTwo :: Input -> Output
partTwo world = length . nub . concat $
                [getAntinodes [0 .. ] dims a1 a2 | antennaGroup <- antennas world,
                                                   a1 <- antennaGroup,
                                                   a2 <- antennaGroup,
                                                   a1 < a2]
    where dims = (height world, width world)
```

Note one more thing here:
- I get the nodes of **ordered** pairs of antennas (a1 < a2). This makes sure that
we don't get the antinode of a pair twice, and that we don't get the antinode of a single antenna (these are pairs of distincts antennas).


## The end part

All that talk about parametric equations just means "basically we move step by step".

I just wanted to talk about maths.
