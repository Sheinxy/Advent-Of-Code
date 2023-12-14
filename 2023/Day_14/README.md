## Day 14

Listen, I am ill today, so I first went for an [awful solution that runs in 25s](./Day_14_slow.hs).
I kept working more on my solution to reduce that time to around 2s afterwards because I was not satisfied.

I am only going to detail that faster solution here (but worry not, both solutions are pretty much the same, just one has easier data structures to handle and runs faster :D)

```hs
data Direction = North | West | South | East deriving (Eq)
data Cycle     = Cycle { start :: Int, values :: [Int] } deriving (Show)

type Input = [String]
type Output = Int

parseInput :: String -> Input
parseInput = lines

rotate90 :: Input -> Input
rotate90 = map reverse . transpose

rotate180 :: Input -> Input
rotate180 = map reverse . reverse

rotateN90 :: Input -> Input
rotateN90 = rotate180 . rotate90

slide :: Direction -> Input -> Input
slide West  = rotate180 . slide East . rotate180
slide South = rotate90  . slide East . rotateN90
slide North = rotateN90 . slide East . rotate90 
slide East  = map slideRow
    where slideRow = intercalate "#" . map sort . splitOn "#"

getLoad :: Input -> Output
getLoad world = sum [i | (i, row) <- zip [1 .. ] (reverse world), char <- row, char == 'O']

partOne :: Input -> Output
partOne = getLoad . slide North

findCycle :: Input -> Cycle
findCycle world = go empty world 0
    where go seen world n | world `member` seen = Cycle (seen ! world) []
                          | otherwise           = Cycle start (getLoad world : nexts)
                          where world' = foldl' (flip slide) world [North, West, South, East]
                                (Cycle start nexts) = go (insert world n seen) world' (n + 1)
            
partTwo :: Input -> Output
partTwo world | 1_000_000_000 <= start cycle = values cycle !! 1_000_000_000 -- As if!
              | otherwise                    = values cycle !! idx
    where cycle    = findCycle world
          cycleLen = (length . values) cycle - start cycle
          idx      = (1_000_000_000 - start cycle) `rem` cycleLen + start cycle
```

## The input

Alright, this is already the first I changed between my two solutions: here the parsing is much more simpler!

```hs
type Input = [String]

parseInput :: String -> Input
parseInput = lines
```

I basically keep the input as is! (Well I keep it as a list of rows)

## Sliding stuff

The core gimmick of this puzzle is to slide round rocks around the map. Part two requires to slide them in multiple direction, part one requires to slide them only north.

I claim that the easiet direction to slide rocks towards is East, so let's focus on that first!

### Sliding East

```hs
slide East  = map slideRow
    where slideRow = intercalate "#" . map sort . splitOn "#"
```

In order to slide my whole map West, I can simply slide each row individually! This is due to the fact that rocks only slide alongside their current row, therefore they are not impacted by what happens to the other rows!

In order to slide a row, I start by splitting it in chunks delimited by cubic rocks. For example, I transform:
```
O.O.O.#..O.#.#
```

Into:
```
["O.O.O", "..O.", "."]
```

Now, for each chunk of my row, I simply sort to group every rock on one side and every . on the other!

```
["..OOO", "...O", "."]
```

Now that I have my chunks, I can join them back together with cube rocks!

```
"..OOO#...O#."
```

And voilà! I have made every rock on the row slide westbound! Now rince and repeat for every row and it's done!

### But... I wanted to slide north :C

When you think about it, directions are just a question of perspective. Sliding North is the same thing as sliding East but looking at the world with your head titlted 90 degrees right!

So if we can somehow rotate the world, we simply need to rotate once, slide East, and rotate once in the opposite direction!

Let's look at the rotation problem then:

```
ABCD
EFGH
```

Rotate this 90 degrees right once:
```
EA
FB
GC
HD
```

Looking at this, it is obvious what happens: rows become columns (ABCD went from a row to a column), and their order is flipped (ABCD went from the first row to the last)!

Swapping column and rows is a pretty common technique called [transposing](https://en.wikipedia.org/wiki/Transpose).

Flipping the order is simply reversing each transposed row!

So we get:

```hs
rotate90 :: Input -> Input
rotate90 = map reverse . transpose
```

Now, the problem here is still present for rotating south and west. The solution: rotate more!

In fact, let's first rotate once more to get a 180 rotation, and once again to get a negative 90 rotation!

```hs
rotate180 :: Input -> Input
rotate180 = rotate90 . rotate90

-- UPDATED:
rotate180 :: Input -> Input
rotate180 = map reverse . reverse
```
-> 
```
HGFE
DCBA
```

```hs
rotateN90 :: Input -> Input
rotateN90 = rotate180 . rotate90
```
->
```
DH
CG
BF
AE
```

And now, sliding in other directions is simply rotation one way, sliding East, and rotation the other way!

```hs
slide :: Direction -> Input -> Input
slide West  = rotate180 . slide East . rotate180
slide South = rotate90  . slide East . rotateN90
slide North = rotateN90 . slide East . rotate90 
```

## Getting the load

We still need one last thing before solving part one: getting the load of every rounded rock.

Well that is actually straighforwars:
```hs
getLoad :: Input -> Output
getLoad world = sum [i | (i, row) <- zip [1 .. ] (reverse world), char <- row, char == 'O']
```

I go through the whole grid starting from the last row, and I add the row number once for each 'O' that is present in it!

## Alright, now let's talk about cycles

There is one very known rule here:
 - If there is a big number and a cycle involved in a puzzle, then the iteration will enventually land onto a cycle!

Now, the question is: how to find that cycle?

Here is my answer:
```hs
data Cycle     = Cycle { start :: Int, values :: [Int] } deriving (Show)

findCycle :: Input -> Cycle
findCycle world = go empty world 0
    where go seen world n | world `member` seen = Cycle (seen ! world) []
                          | otherwise           = Cycle start (getLoad world : nexts)
                          where world' = foldl' (flip slide) world [North, West, South, East]
                                (Cycle start nexts) = go (insert world n seen) world' (n + 1)
```

Two things about the cycle interest me:
 - The number of iteration before first stepping foot inside the cycle
 - The values that are found before the first cycle loop

With that I can always find a value for a given number of iteration, even if it is before the first cycle loop (unlikely here but we never know!)

Now, in order to find those two things, I simply use a Map, mapping each state of my grid/map/world/input (I suck at naming things, haven't you noticed?) to the iteration number coresponding to it.

For example, iteration 0 is my input without any sliding.

When I notice that my world is a key in my Map, this means that I'm back at the start of the loop. The loop therefore starts after the number of iteration corresponding to that world.

If the current iteration is a new one, then I simply add its load to my list of values.

With the sample input, here is what this gives me:
```
O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....
```
->
```hs
Cycle {start = 3, values = [104,87,69,69,69,65,64,65,63,68]}
```

This means that my cycle starts at index 3 (the second 69). The values after that index (itself included) just loop forever. The values before happen once during the first 3 iterations.

Now, in order to find the 1000000000th iteration, I simply need to drop the first values outside of my cycle, and do (iterations left) % (cyclen length):

```hs
partTwo :: Input -> Output
partTwo world | 1_000_000_000 <= start cycle = values cycle !! 1_000_000_000 -- As if!
              | otherwise                    = values cycle !! idx
    where cycle    = findCycle world
          cycleLen = (length . values) cycle - start cycle
          idx      = (1_000_000_000 - start cycle) `rem` cycleLen + start cycle
```

And that's it folks!
