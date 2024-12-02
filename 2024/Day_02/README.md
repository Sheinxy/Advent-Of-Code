## Day 02

Today was simple and fun!

![Yippee](https://cdn.discordapp.com/attachments/690279803701821703/1313100922460704839/yippee.png?ex=674ee808&is=674d9688&hm=797a36aae9d2dd4cc3b1cd00bc305f9c0a1574f2b3ffc7ded1e37d0b90654a13&)

## The input:

The input is pretty simple to parse, each line is just a list of numbers separated by
spaces:

```
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
```

In order to parse it I simply get each lines, I split each line by spaces
and I convert each element of the resulting strings to numbers:

```hs
parseInput :: String -> Input
parseInput = map (map read . words) . lines
```

## Part 1:

### The problem:

In part 1 we have to find which reports are safe and which aren't.

A safe report is a report for which numbers are sorted (either incresingly or decreasingly)
and the difference between two numbers is at least 1 and at most 3.

### The idea:

These two conditions can be checked in one go:
- If the levels are sorted in increasing order, then the difference between two
  consecutive numbers must be between 1 and 3
- If the levels are sorted in decreasing order, then the difference between two
  consecutive numbers must be between -1 and -3

If two consecutive numbers have a difference that is not between that interval, or that this difference is between a different interval than the difference between two other consecutive numbers, then the report is not safe.

### The code:

I start by computing the difference between consecuting numbers by zipping each report and its tail with the subtracting operator.

That is:
```hs
report = [7, 6, 4, 2, 1]
zipWith (-) report $ tail report
-- This gives [-1, -2, -2, -1]
```

Then, I filter each resulting list by checking that either:
- all differences are between 1 and 3
- all differences are between -3 and -1
That is, I filter each list by checking that it represents a safe report.

And I then take the resulting length as my final result:

```hs
partOne :: Input -> Output
partOne = length . filter isSafe . map computeDiff
    where computeDiff report = zipWith (-) report $ tail report
          isSafe diff = all (\x -> 1 <= x && x <= 3) diff || all (\x -> -3 <= x && x <= -1) diff
```

## Part 2:

### The problem:

The definition of a safe report is still the same, albeit there is now a small tolerence:
If a report can be made safe by removing one single element, then the report is considered safe.

### The idea:

In order to solve that, I simply generated for each report the subreports where at most one element was removed.

Then I simply apply the same computations I did for part one to each subreport, and I filter by keeping the report where at least one subreport is safe.

### The code:

In order to generate the sublists, I make use of the [`subsequences`](https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-List.html#v:subsequences) function:

```hs
subsequences [7, 6, 4, 2, 1]
-- [[],[7],[6],[7,6],[4],[7,4],[6,4],[7,6,4],[2],[7,2],[6,2],[7,6,2],[4,2],[7,4,2],[6,4,2],[7,6,4,2],[1],[7,1],[6,1],[7,6,1],[4,1],[7,4,1],[6,4,1],[7,6,4,1],[2,1],[7,2,1],[6,2,1],[7,6,2,1],[4,2,1],[7,4,2,1],[6,4,2,1],[7,6,4,2,1]]
```

This function generates all combinations of keeping or removing each element from the original list.

What I want now is to only keep the combinations where at most one element was removed. In order to do that, I simply filter by comparing the original length and the remaining length:
```hs
computePossibilities l = filter (\x -> abs (length l - length x) <= 1) $ subsequences l
```

Now, I simply apply this computation to each report, and I check that any subreport is safe.

```hs
partTwo :: Input -> Output
partTwo = length . filter (any (isSafe . computeDiff) . computePossibilities)
    where computePossibilities l = filter (\x -> abs (length l - length x) <= 1) $ subsequences l
          computeDiff report = zipWith (-) report $ tail report
          isSafe diff = all (\x -> 1 <= x && x <= 3) diff || all (\x -> -3 <= x && x <= -1) diff
```

## The end part:

I didn't know about the subsequences function, I actually went my way to look for it!

This is what I love about the Advent Of Code: even after four years of doing this in Haskell,
I'm still (re)discovering new things all the time!
