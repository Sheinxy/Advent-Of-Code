## Day 05

Guess who woke up at 6:30AM to do his advent of code today!

![Happy cat](https://st2.depositphotos.com/2412817/5730/i/950/depositphotos_57307411-stock-photo-very-tired-cat-on-a.jpg)

Today was very simple!

## The input:

So, we have to parts to parse in the input:
- The first parts is composed of two numbers separated by a | on each line
- The second part is a comma-separated list of numbers on each line

The two parts are separated by an empty line.

I span the lines until I get an empty line, this gives me two lists:
the first contains the lines with a '|', the second gives me the empty line and
the rest of the lines.

In order to parse the lines with a '|', I split the input on the '|' and I read
both sides as integers, giving me a pair of integers.

In order to parse the comma-separated lists, I simply add "[" and "]" on both ends
and I let Haskell read it into a list of integers :)

```hs
parseInput :: String -> Input
parseInput = getInput . break null . lines
    where getInput (orders, _ : numbers) = (map readOrder orders, map readNumber numbers)
          readOrder n = (read (takeWhile (/= '|') n), read (tail $ dropWhile (/= '|') n))
          readNumber n = read ("[" ++ n ++ "]") :: [Int]
```

## Part 1:

### The problem:

We need to figure out if each list is ordered according to the ordering rules.

The ordering rule "X|Y" states that if both X and Y are present in the list,
X must come before Y.

Then, we need to find the correctly sorted lists, and sum their middle elements

### The solution:

In order to solve that, we can simply define this ordering rule in Haskell:

```hs
getCorrectOrder :: [(Int, Int)] -> Int -> Int -> Ordering
getCorrectOrder orders a b
            | (a, b) `elem` orders = LT
            | (b, a) `elem` orders = GT
            | otherwise            = EQ
```

This function checks that the pair composed of a and b is present in the ordering
rules, and tells LT if a comes first, GT if it comes second.

Now we can sort the list using this ordering rule to get the rightly ordered list.

If the ordered list is the same as the original list, the original is correct and
we can use its middle point!

```hs 
middle :: [a] -> a
middle l = l !! (length l `div` 2)

partOne :: Input -> Output
partOne (orders, input) = sum [middle a | (a, b) <- zip ordered input, a == b]
    where ordered = map (sortBy $ getCorrectOrder orders) input
```

## Part 2:

### The problem:

Now we focus on the unordered lists. We need to order them and sum up their middle
elements.

### The solution:

We already ordered them. We just need to sum up their middle element!

```hs
partTwo :: Input -> Output
partTwo (orders, input) = sum [middle a | (a, b) <- zip ordered input, a /= b]
    where ordered = map (sortBy $ getCorrectOrder orders) input
```

## The end part:

The way I made part 1 made part 2 really really really really easy :3c
