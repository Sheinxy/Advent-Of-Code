## Day 01

Good morning (or afternoon [or evening]) everyone, and welcome back to my annual
Advent Of Code in Haskell write-ups!

This year is going to be quite busy for me, so I don't know how much I'll be able
to do this every day, but I'll try my best!

So, without further ado, let's start the write-up:

## Input:

We have two lists: a left one and a right one
```
3   4
4   3
2   5
1   3
3   9
3   3
```

I start by getting each line from the input, which I split on "   " to get two
strings for each line corresponding to each number. I convert each string to an Int,
giving me for each line a list of two numbers: [[3, 4], [4, 3], [2, 5]...]
Then I transpose the list to get a list of columns instead of a list of lines:
[[3, 4, 2, 1, 3, 3], [4, 3, 5, 3, 9, 3]]

```hs
parseInput :: String -> Input
parseInput = transpose . map (map read . splitOn "   ") . lines
```

## Part 1:

### The problem:

We need to pair the number of each list by their rank (in increasing order) of
their respective list.
For example, 1 from the left list is paired with 3 from the right list, 2 with 3 etc.

### The solution:

In order to do that, I start by sorting each list. I then zip the two resulting lists together
by subtracting the element from the first list by the element from the second list.

Because distances have to be positive value, I then take the absolute value of each resulting subtraction,
and I sum the results:

```hs
partOne :: Input -> Output
partOne = sum . map abs . uncurry (zipWith (-)) . toPair . map sort
    where toPair [a, b] = (a, b)
```

The `toPair` function transforms my input, which is a list of lists, into a pair of lists,
allowing me to use the `uncurry` function to perform the zipWith, and keep everything in point-free
notation (because it's ✨fancy✨ :3c)

## Part 2:

### The problem:

We now need to count each time an element from the left list appears in the second list,
and multiply that count by the element.
For example, 3 appears 3 times in the right list, so for each 3 in the left list we get the result 3 * 3 = 9. 4 appears 1 time, so it becomes 4 * 1 = 4. etc.

In order to do that, I simply count the number of times each element appears in the list and multiply that count by the element itself. I then sum all the results.

In order to count the number of times each element appears, I filter my right list by only keeping
the items that match the element from the left list, and I get the length of the resulting list.

```hs
partTwo :: Input -> Output
partTwo [a, b] = sum $ map (\x -> x * (x `countElem` b)) a
    where countElem a = length . filter (== a)
```

This is definitely not optimised:
I go through the whole list for every element, even elements I've already encountered.

But considering the small size of each lists, it runs decently well, so it's quite fine!


## The end part

I'm so hyped for this year's Advent Of Code, it's basically the only time of the year when I code in Haskell and I love it so much!

I hope this year won't be too difficult considering that it might not be easy for me to work on these puzzles every day, but I'll get we'll see!
