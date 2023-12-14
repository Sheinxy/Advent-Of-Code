## Day 13

Day 10 was hard. Day 11 was easy. Day 12 was hard. Today was easy.

![There must be a pattern](https://i.kym-cdn.com/entries/icons/facebook/000/037/612/Untitled-1.jpg)

Anyway, hello everyone (is anyone actually reading this? do let me know if you do), I hope you had a good day!

```hs
type Input = [[String]]
type Output = Int

parseInput :: String -> Input
parseInput = map lines . splitOn "\n\n"

getMirrorDiffs :: String -> [Int]
getMirrorDiffs row = map (length . filter not) . zipWith (zipWith (==)) starts $ ends -- For each (left, right) pair, find the characters that differ and count the number of different characters
      where starts = (map reverse . tail . inits) row -- The different left  parts, mirrored
            ends   = (init        . tail . tails) row -- The different right parts

getMirrorRow :: Int -> [String] -> Int
getMirrorRow smudges row = 1 + fromMaybe (-1) (smudges `elemIndex` results) -- Find the row number that has the right number of smudges
           where results = map sum . transpose . map getMirrorDiffs $ row -- Number of smudges for each vertical line row number

solve :: Int -> Input -> Output
solve n grids = sum [getMirrorRow n g + 100 * (getMirrorRow n . transpose $ g) | g <- grids]

partOne :: Input -> Output
partOne = solve 0

partTwo :: Input -> Output
partTwo = solve 1
```

Let's start explaining!

## The input:

```hs
type Input = [[String]]
type Output = Int

parseInput :: String -> Input
parseInput = map lines . splitOn "\n\n"
```

My input simply is my list of patterns (which I like to call grids. Everything that looks like a 2D grid I call a grid 😸).

To parse it, I simply first get each pattern as a single string containing "\n"s by splitting on every "\n\n" (sequence of two linebreaks).

Then, for each pattern, I split into lines. That's all :)

## The solution:

### Step 1: Get the number of different characters when mirroring along each possible column

To make the problem easier to solve, I first focus on wanting to get the potential reflexion line indices of a single row:

```hs
getMirrorDiffs :: String -> [Int]
getMirrorDiffs row = map (length . filter not) . zipWith (zipWith (==)) starts $ ends -- For each (left, right) pair, find the characters that differ and count the number of different characters
      where starts = (map reverse . tail . inits) row -- The different left  parts, mirrored
            ends   = (init        . tail . tails) row -- The different right parts
```

I start by splitting my row into two lists: my left sides (starts) of the reflexion line and my right sides (ends).

For example, if my row is "#.##..##.", the first three elements of starts are ["#", ".#", "#.#"], and the first three elements of ends are [".##..##.", "##..##.", "#..##."].

Note two things here:
 - I ditch the case when one of the sides is empty as it is not a suitable for line reflexions
 - I reverse the left sides to make it easier to check if it mirrors the right side.

Now that this is done, I want to compare each pair to see how much they mirror each other (I want to compare "#" with ".##..##.",  ".#" with "##..##." etc.)

To do that, I use zipWith once to apply a function on each pair. The funny part is the function I apply: another zipWith.

This other zipWith is actually comparing each pair of characters, checking if they are equal or not. So in the case of my third pair, it will check: '#' with '#', '.' with '.' and '#' with '.'

Next, for each side pair comparison, I find the number of different characters.

With the example I gave earlier, this gives me [1,1,1,3,0,3,0,1]. This means that:
 - If the line reflexion is after column 1, then there would be exactly one character that is different between the left and right sides of the line.
 - Same after columns 2 and 3
 - Column 4 would have 3 differing characters
 - Column 5 would have none
 etc.

### Step 2: Now do that for every row and find the most suitable line.

Now that I am able to do that for one row, I need to do it for every row!

```hs
getMirrorRow :: Int -> [String] -> Int
getMirrorRow smudges row = 1 + fromMaybe (-1) (smudges `elemIndex` results) -- Find the row number that has the right number of smudges
           where results = map sum . (transpose . map getMirrorDiffs) $ row -- Number of smudges for each vertical line row number
```

However, one small problem arises when I simply map each row to my getMirrorDiffs function:
 - I get a matrix where each column represent the number of differing characters on each row if the reflexion line was put at that index.

This is a problem because it's really annoying to focus on column when dealing with a list of lists.

No worries! I can simply [transpose](https://en.wikipedia.org/wiki/Transpose) my matrix so that rows become columns and columns become rows! (Yippee!)

Now, knowing the individual number of smudges on each row for a specific reflexion line index isn't really useful. What I want to know is the total number of smudges that would arrive if the reflexion line was put at that index. In order to do that, I simply need to sum all of those individual numbers into one!

Now, the constant that I call results is just a list mapping each reflexion line index to the number of smudges they would cause!

And to finish, I now simply look at the index that would give me the right number of smudges (and I add one because the puzzle starts indices at 1). I default to -1 to get 0 as my answer if no reflexion line would satisfy that.

Part one would be finding the line giving 0 smudges (perfect reflexion) and part two would be finding the one giving one smudge!

### Step 3: Call that on everything and add the results! Wait... Didn't we forget something?

Now, so far I've only talked about rows. Sure, this is great, but what about columns??!!

Well, remember earlier, when I said that it was annoying to deal with columns, but easy to deal with rows?

Well this is still the case here, and the answer is still the same: transpose everything!

e.g, this is hard to deal with:
```
#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#
```

The reflexion is between the fourth and fifth column, but we only know how to handle rows!

Now, what about that input instead?
```
##.##.#
...##..
..####.
..####.
#..##..
##....#
..####.
..####.
###..##
```

Here, the reflexion is between the fourth and fifth row!
In fact, the n-th column in our former input is now the n-th row! Therefore we can simply use the old method that works on rows!

And that's what we do:

```hs
solve :: Int -> Input -> Output
solve n grids = sum [getMirrorRow n g + 100 * (getMirrorRow n . transpose $ g) | g <- grids]
```

Now to solve for a specific part, we simply take n to be the number of smudges we want, and voilà!
