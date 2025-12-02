## Day 01

Welcome back everyone! My name is Sheinxy, I have an unhealthy love for Haskell,
you know the drill, let's get started! >:3

## Parsing

### The input

The input for this puzzle is pretty simple:
- We have a bunch of instructions with the following format : `[RL]\d+`
- R means turning the dial right by the specified amount
- L means turning it left (backwards) by the specified amount

So, this could be parsed as a list of numbers. Right instructions are positive
numbers and left instructions are negative:

```hs
type Input = [Int]

parseInput :: String -> Input
parseInput = map parseLine . lines
    where parseLine ('R' : num) = read num
          parseLine ('L' : num) = - (read num)
          parseLine _ = error "Impossible case, check that your input is valid"
```

This is not necessarily elegant, but it's simple and clean. All it does is
getting each line from the input and parsing it by reading the number,
negating it if the letter is 'L'.

## Step 1

### The problem

We have a dial that starts at 50, we turn the dial following the instructions,
how many times do we fall onto a '0' after turning it?

### The solution

This is simple:
- Turning the dial means adding a number to the current one
- Ending up on 0 means having a multiple of 100 as the result

Because this is just a question of modular arithmetic, we don't really need
to worry about when we perform the `mod` 100 ((x `mod` 100 + y) `mod` 100 is the same as (x + y) `mod` 100).

The way I did this was by using the wonderful [scanl function](https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html#v:scanl).

This function is pretty simple:
- Start with a default value for the accumulator
- Take a function and a list of values
- Sequentially apply the function to the accumulator and the values from the list
- The result is a list containing all the accumulator's values

So, in our puzzle, we can use scanl to simulate the turning of the dial (not caring about the modulo for now)

Once we have that, we simply need to [count](https://hackage-content.haskell.org/package/ghc-9.12.2/docs/GHC-Utils-Misc.html#v:count) how many numbers are multiples of 100 (getting a 0 on the dial).

```hs
type Output = Int

partOne :: Input -> Output
partOne = count ((== 0) . (`mod` 100)) . scanl (+) 50
```

## Step 2

### The problem

We actually don't want to count how many times we end up on 0, but how many times we pass 0.

### The solution

I didn't go for a very elegant solution, but for one that is simple and that works.

Let's first time by addressing the elephant in the room: What happens
if I turn the dial by a distance of 1050?

Well, the first 1000 steps are actually "useless". In fact, every 100 step is "useless" because
turning the dial by 100 means doing a full rotation. However, doing a full rotation implies passing 0.

Therefore, we can count all the full rotations as passed 0s. We can get this number by dividing the distance by 100.

```
⚠️  What about negative numbers? 

Negative numbers aren't really much of a special case.

Turning the dial -100 means doing a full revolution, -200 two full, etc.
So dividing this by 100 still works, we can simply get the absolute value later.

The real problem is: which division to use?

The are two divisions:
[div](https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html#v:div) and [quot](https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html#v:div)

div truncates towards negative infinity and quot towards zero. In our case, quot is what we want.
```

Now, the next question is: Does turning the dial by the remaining distance passes 0?

- If the current number is 0, then the answer is always no (as the remainder is less than 100)
- Otherwise, we can compare the distance to the current number to 0 with the remainder. If the remainder is less than or equal to the distance to 0 in the negative direction or if it is greater than or equal to the distance to 0 (100) in the positive direction, then we pass 0.

Now, I am once again reusing scanl, but my accumulator is a tuple of the form `(number of times 0 was passed to get here, current number)`.

I can then simply sum up the number of times 0 was passed to get the solution.

```hs
partTwo :: Input -> Output
partTwo = sum . map fst . scanl step (0, 50)
    where  step (_, cur) rot = (abs turns + fromEnum clicks, (cur + left) `mod` 100)
            where (turns, left) = rot `quotRem` 100 -- Gives the number of complete turns and the remainder for the last turn
                  clicks = cur /= 0 && (left >= 100 - cur || left <= -cur) -- Gives whether or not the last turns passes or falls on 0
```

## Conclusion

I am one day late already lol. It's fine, there are only 12 days so I have plenty of time to get back on track :3c

