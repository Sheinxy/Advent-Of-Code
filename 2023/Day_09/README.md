## Day 09

Get ready for the easiest day ever!

Parsing the input is simple:
```hs
type Input = [[Int]]
type Output = Int

parseInput :: String -> Input
parseInput = map (map read . words) . lines
```
Each line represents a sequence of number, that  I represent as a list. To get the numbers on a line, I simply split by spaces and map each number string to its integer representation. Done.

Now for the actual code, I am barely going to split my explanation of part one and part two because they are fricking similar:
```hs
generateSubsequence :: [Int] -> [[Int]]
generateSubsequence = takeWhile (not . all (== 0)) . iterate getDiffs
    where getDiffs l = zipWith (-) (tail l) l

partOne :: Input -> Output
partOne = sum . map (foldr ((+) . last) 0 . generateSubsequence)

partTwo :: Input -> Output
partTwo = sum . map (foldr ((-) . head) 0 . generateSubsequence)
```

The most interesting part is
```hs
generateSubsequence :: [Int] -> [[Int]]
generateSubsequence = takeWhile (not . all (== 0)) . iterate getDiffs
    where getDiffs l = zipWith (-) (tail l) l
```
The getDiffs function gives the list of deltas in a list of numbers.
For example, if my list of numbers is [2, 4, 9], then getDiffs will yield [2, 5].

The way it works is by getting two list: [4, 9] and [2, 4, 9], and zipping them with the (-) operator: [4 - 2, 9 - 4], which is [2, 5]

I iterate this function over my initial list (so for example, if the 0th iteration is [0, 2, 4, 6], the second [2, 2, 2], the third [0, 0] etc.).

I keep each iteration in the list until the iteration only has 0 in it, so here I would get [[0, 2, 4, 6], [2, 2, 2]].

For the first part:
```hs
partOne :: Input -> Output
partOne = sum . map (foldr ((+) . last) 0 . generateSubsequence)
```

After generating my subsequences for a line, I simply add together each last element of these subsequences. I sum all the results together to get my result

For part two:
```hs
partTwo :: Input -> Output
partTwo = sum . map (foldr ((-) . head) 0 . generateSubsequence)
```
After generating my subsequences for a line, I simply subtract together each first element of these subsequences. I sum all the results together to get my result

Easy peasy lemon squeezie 🐈‍⬛
