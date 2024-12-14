## Day 14

Welcome to "let's just print everything and look-up manually"'s day!

## The input:

Each line represents a robot with a position and a velocity.

I first defined a data type representing that robot:

```hs
data Robot = Robot { position :: (Int, Int), velocity :: (Int, Int) } deriving (Show)
```

Then, for each line I extract the position and velocity part of the line,
and I convert them to tuple of Int:

```hs
parseInput :: String -> Input
parseInput = map parseRobot . lines
    where parseRobot :: String -> Robot
          parseRobot line = Robot pos vel
            where [_ : p : v : _] = line =~ "p=([0-9]+,[0-9]+) v=(-?[0-9]+,-?[0-9]+)" :: [[String]]
                  pos             = read ("(" ++ p ++ ")")
                  vel             = read ("(" ++ v ++ ")")
```

## Part 1:

### The problem

I want to know the robots' positions after 100 seconds.

### The solution

I first define a function that computes a robot's position after a given number of seconds.

A robot's position is computed with the following formulas:
- x' = (x + t * vx) mod width
- y' = (y + t * vy) mod height

```hs
size :: (Int, Int)
size = (101, 103)

move :: Int -> Robot -> Robot
move seconds r = r'
    where (width, height) = size
          (x,   y) = position r
          (dx, dy) = both (* seconds) $ velocity r
          pos'     = ((x + dx) `mod` width, (y + dy) `mod` height)
          r'       = r { position = pos' }
```

Now, I need to be able to sort a robot by its quadrant. That is simply comparing
its coordinates to the middle row and column:

```hs
isInQuadrant :: Robot -> Int
isInQuadrant (Robot (x, y) _)
    | x < midX && y < midY = 1
    | x > midX && y < midY = 2
    | x < midX && y > midY = 3
    | x > midX && y > midY = 4
    | otherwise            = 0
    where (midX, midY) = both (`div` 2) size
```

Quadrant 0 corresponds to the middle line or column.

Now I can compute the safety factor by simply getting the number of robots in each quadrant
and multiplying these numbers together:

```hs
computeSafetyFactor :: Int -> Input -> Output
computeSafetyFactor seconds = product . 
                              map length .
                              group . sort . 
                              filter (/= 0) .
                              map (isInQuadrant . move seconds)

partOne :: Input -> Output
partOne = computeSafetyFactor 100
```

## Part 2

### The problem

I have to find when these robots form a christmas tree.

I have no idea what that christmas tree looks like, nor how big it is.

### The solution

I start by finding the string representation of the grid after a given number of seconds.

I represent robots with "#" and empty tiles with " ".

```hs
getGridAfter :: Int -> Input -> [String]
getGridAfter seconds rs = [ [ getRepresentingChar x y | x <- xs ] | y <- ys ]
    where (width, height) = size
          rs' = map (move seconds) rs
          xs   = [0 .. width  - 1]
          ys   = [0 .. height - 1]
          getRepresentingChar x y | any (\r -> position r == (x, y)) rs' = '#'
                                  | otherwise                            = ' '
```

Then, I print a lot of grids...

```hs
partTwo :: Input -> IO ()
partTwo input =  for_ [0 .. fst size * snd size] $ \i -> do
                    print i
                    mapM putStrLn  . getGridAfter i $ input
```

And now, I just launch that and I look manually.

In order to be more efficient (or less unefficient), I use a grep to find lines with multiple "#" (as it will probably give me a better chance of finding the tree):

```hs
./Day two input | fgrep -B 101 "#####" > result
```

Read that with less, and enjoy you horrible moment

## The end part

I don't like puzzles when I don't know what I'm supposed to look for :c
