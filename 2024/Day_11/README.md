## Day 11

Premature optimisation is the root to solving part 2 in 30 seconds.

![Smart cat](https://mir-s3-cdn-cf.behance.net/project_modules/disp/61140e36475251.560643695918f.jpg)

## The input

We have a list of numbers, separated by spaces.

You know the drill by now: we split by spaces and we convert the strings to numbers:
```hs
    where numbers           = map read . words $ input
```

Now, I am not going to stop here:

This is an advent of code puzzle, therefore I know there is going to be a lot of numbers,
and some of these numbers will be present multiple times. Therefore, I am going to "compress"
my input into a Map where the keys are the numbers themselves, and the value their count:

```hs
type Input  = Map Int Int

parseInput :: String -> Input
parseInput input = Map.fromList compressedNumbers
    where numbers           = map read . words $ input
          compressedNumbers = map (\l -> (head l, length l)) . group . sort  $ numbers
```

In order to get this, I sort the numbers in order to group equal numbers together.

Then, I transform each group into a tuple where the first entry is the number itself, and the second
is the number of times this number appeared in the list.

## Part 1

### The problem

We want to "blink", meaning changing our input by following three rules on each element of this input:
- If the number is 0, it becomes a 1
- If the number is of even length, we split it in two numbers
- Otherwise, we multiply it by 2024

### The solution

We need to apply this transformation on every stone of our input. Because our input is compressed,
each resulting stone of our input will have the same count as the original stone.

For example, if we're dealing with the stone (1234, 4), which means 4 stones numbered 1234,
then the result will be two stone (12, 4) and (34, 4), therefore four stones labelled 12 and four labelled 34.

We can create our new Map by transforming each stone of the original map that way:
```hs
blink :: Input -> Input
blink = Map.foldrWithKey transformStone Map.empty
    where addStone count Nothing       = Just count
          addStone count (Just count') = Just (count + count')
          transformStone stone count res
            | stone == 0               = Map.alter (addStone count) 1 res
            | (even . length) stoneStr = Map.alter (addStone count) rightHalf $ Map.alter (addStone count) leftHalf res
            | otherwise                = Map.alter (addStone count) (stone * 2024) res
            where stoneStr = show stone
                  (leftHalf, rightHalf) = (read *** read) $ splitAt (length stoneStr `div` 2) stoneStr
```

We start with an empty map, and we apply the transformStone function to every stone of our input.

When applying a rule, we are altering the result Map using the addStone function:
- If the map already contains stones with this label, we update the count for that type of stone by adding the two counts.
- Otherwise, we add this stone with the count inside our resulting map.

Now, we simply need to do that 25 times and sum all the counts together:
```hs
partOne :: Input -> Output
partOne = sum . Map.elems . (!! 25) . iterate blink 
```

## Part 2

### The problem

Do that 75 times now.

### The solution

Do that 75 times now.

```hs
partTwo :: Input -> Output
partTwo = sum . Map.elems . (!! 75) . iterate blink 
```

## The end part

```
      --------Part 1--------   --------Part 2--------
Day       Time   Rank  Score       Time   Rank  Score
 11   05:53:03  26591      0   05:53:34  16802      0
```
