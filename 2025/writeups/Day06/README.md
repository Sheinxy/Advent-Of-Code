## Day 06

That's it? That was just a parsing problem!

## The Input

Actually nevermind, the two parts were about parsing the input so
there is not "Parsing the input" part today.

## Part One

### The Problem

We have a list of maths problems that looks like this:

```
123 328  51 64
 45 64  387 23
  6 98  215 314
*   +   *   +
```

The problems are made out of a bunch of numbers and an operation to apply to
them.

They are arranged vertically. For example, the first problem is 123 * 45 * 4.

### The Solution

Let's represent this input as a list of operations and numbers.

This way, computing the answer is just folding the numbers with the operation
and summing the results:
```hs
type Input = [(Int -> Int -> Int, [Int])]
type Output = Int

compute :: Input -> Output
compute = sum . map (uncurry foldl1)
```

To parse the input, let's start by getting the lines, that is (this is done with lines):
```
123 328  51 64
 45 64  387 23
  6 98  215 314
*   +   *   +
->

["123 328 51 64", ... "*   +   *   +"]
```

Next, let's get the columns by splitting at each ' ' (this is done with map words):
```
["123 328 51 64", ... "*   +   *   +"]
->
[["123", "328", "51", "62"], ... ["*", "+", "*", "+"]]
``` 

Now, we want to read everything vertically. Therefore, we want to use each column
as our lines and each line as our columns. This is called [transposing](https://en.wikipedia.org/wiki/Transpose).
(This is done with transpose)
```
[["123", "328", "51", "62"], ... ["*", "+", "*", "+"]]
->
[["123", "45", "6", "*"], ... ["64", "23", "314"]]
```

Finally, we split each list in two parts: the operation, which we'll parse into
the corresponding haskell function, and the numbers which we'll convert to ints.
(this is done by reversing the list, getting the head and the tail in a tuple, and calling the parsing function on each element of the tuple)
```
[["123", "45", "6", "*"], ... ["64", "23", "314", "+"]]
->
[((*), [6, 45, 123]), ..., ((+), [314, 23, 64]]
```

The numbers have been reversed, but it doesn't matter because addition and multiplication of integers
is [commutative](https://en.wikipedia.org/wiki/Commutative_property).

Put together:
```hs
parseOperator :: String -> (Int -> Int -> Int)
parseOperator "*" = (*)
parseOperator "+" = (+)
parseOperator _   = error "Something went wrong, please check your input."

partOne :: String -> Input
partOne = map ((parseOperator *** map read) . fromJust . uncons . reverse)
        . transpose . map words . lines
```

## Part Two

### The Problem

Actually, problems are not the only thing that are written vertically,
numbers are also written vertically.

The actual puzzle mentions that they are written "right-to-left in columns".

While this is true, I don't think the mention of 'right-to-left' is important,
as the operations are still commutative. The only important thing is that they're
written in columns.

### The Solution

Let's start again by getting the lines.
```
123 328  51 64
 45 64  387 23
  6 98  215 314
*   +   *   +
->

["123 328 51 64", ... "*   +   *   +"]
```

Now, we know that we want to read things column by column, because numbers are written in columns. So let's transpose already:
```
["123 328 51 64", ... "*   +   *   +"]
->
["1   * ", "24  ", "356 ", "    ", ... "623+", "431 ", "  4 "]
```

Now, notice that the problems are already groupped together, and they are separated by
empty elements ("    "). We can group them properly (splitWhen (all (== ' '))):
```
["1   * ", "24  ", "356 ", "    ", ..., "623+", "431 ", "  4 "]
->
[["1   * ", "24  ", "356 "], ..., ["623+", "431 ", "  4 "]]
```

What we've done so far is pretty simple: read the input column by column and
groupping together each problem.

Now, notice something nice about the input:
the operation is always at the end of the first column.

All we have to do now is to parse the operation by getting the last element of the first column,
and parsing the numbers by just parsing each column, just making just to remove the operation from the first column.

```hs
partTwo :: String -> Input
partTwo = map parseProblem . splitWhen (all (== ' ')) . transpose . lines
    where parseProblem [] = error "Something went wrong, please check your input."
          parseProblem (x : xs) = (parseOperator [last x], map read $ init x : xs)
```

## Conclusion

This was nice :)
