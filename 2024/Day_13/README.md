## Day 13

Let's do some more maths!

![Sheinxy does maths](https://pbs.twimg.com/media/FxKqE-rWAAAHqFS?format=png&name=small)

## The input

In order to parse the input, I split it by chunks separated by two line breaks.

These chunks have the form:
```
Button A: X\+([0-9]+), Y\+([0-9]+)
Button B: X\+([0-9]+), Y\+([0-9]+)
Prize: X=([0-9]+), Y=([0-9]+)
```

I simply use a Regex to parse each chunk, and I store the resulting values inside two matrices:

The first one represents the A and B coefficients the second one represents the prize's coordinates.

I use a Matrix of Rationals instead of Int for reasons I'll explain later.

```hs
type Input = [(Matrix Rational, Matrix Rational)]

parseInput :: String -> Input
parseInput = map parseMachine . splitOn "\n\n"
    where parseMachine :: String -> (Matrix Rational, Matrix Rational)
          parseMachine machine = (coefs, result)
            where [_ : numStr] = (machine =~ 
                                "Button A: X\\+([0-9]+), Y\\+([0-9]+)\n\
                                \Button B: X\\+([0-9]+), Y\\+([0-9]+)\n\
                                \Prize: X=([0-9]+), Y=([0-9]+)") :: [[String]]
                  numbers = map (fromIntegral . read) numStr
                  coefs   = fromList 2 2 $ take 4 numbers
                  result  = fromList 1 2 $ drop 4 numbers
```

## Part 1 and 2


### The problem
Now, what we want to do is basically solve the following system of equations:
- Xa * Ka + Xb * Kb = Xtarget
- Ya * Ka + Yb * Kb = Ytarget

Where Ka and Kb are integers, and the others variables are our input.

### The solution

In order to do that, I saw three possibilities:
- The easy way: use a SAT Solver
- The long way: solve the two [diophantine equations](https://en.wikipedia.org/wiki/Diophantine_equation)
  and find the smallest common solution.
- The smart way: solve the system (using matrices because we're using computers and computers love matrices)

I have decided to do things the smart way:

Suppose we want to solve the following system:
```
Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400
```

I have represented this with the two following matrices:
```
               ( 94 34 )
( 8400 5400 )  ( 22 67 )
```


let's call them respectively Y and A

Then, our system is the following:
- XA = Y

Let's solve it (hard!):
XA = Y => XAA^-1 = YA^-1 => X = YA^-1


So, all we have to do is to invert the A matrix, and multiply Y by A to get our solution!

```hs
computeNumPresses :: (Matrix Rational, Matrix Rational) -> Matrix Rational
computeNumPresses (a, y) = multStd y a'
    where Right a' = inverse a
```

This is why Rational coefficients were important: I can invert the matrix without losing precision

Now, let's note here that I'm assuming that the matrix is invertible.
If it weren't then there wouldn't be any solution (even non-integer ones), which is unlikely.

However, if your input does have non-invertible matrices, it's fine, simply return a default value which has non integer coefficient and enjoy the rest of this writeup! :)


Now that I have solved this problem, I need to compute the token price:
- That is multiply the number of times A is pressed by 3, and the number of times B is pressed by 1.

But what about non-integer solutions ?

Well, if my coefficient is not an integer, this means that the denominator is not 1, therefore let's put its price to 0 in order to not count it.


```hs
getTokenPrice :: Matrix Rational -> Integer
getTokenPrice a
    | isIntegerSolution = sum $ zipWith (*) (map numerator values) [3, 1]
    | otherwise         = 0
    where values = toList a
          isIntegerSolution = all ((== 1) . denominator) values
```

Now, we compute the number of presses of each button for each machine, we get the token price for each solution,
and we add them all together!

```hs
partOne :: Input -> Output
partOne = sum . map (getTokenPrice . computeNumPresses)
```

For part two, we do the same, but we add an offset to the target coordinates beforehand:

```hs
addOffset :: Matrix Rational -> Matrix Rational
addOffset = elementwise (+) (fromLists [[10_000_000_000_000, 10_000_000_000_000]])

partTwo :: Input -> Output
partTwo = partOne . map (second addOffset)
```

## The end part

That was probably one of the easiest days so far tbh
