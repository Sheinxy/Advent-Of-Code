## Day 03

Get ready for a flash writeup.

## The Input

The input is pretty simple to parse.

We have a bunch of lines containing numbers. We don't really need to convert
each digit as an int.

So the parsing is just getting each line:

```hs
type Input = [String]

parseInput :: String -> Input
parseInput = lines
```


## Part all of them at the same time because I had the right idea right away


### The Problem(s)

We want to find the largest number we can make using 2 then 12 digits on a line.

The catch is that we need to pick these numbers in the same order they appear on
the line.

This means that if we have `19`, the largest number 2-digits is 19 and not 91,
because we need to keep the order intact.

### The Solution

When comparing two numbers with the same number of digits, the biggest number
is the one with the higest most significant digit (the leftmost digit). (... I'm sorry, I feel like I am pointing out the obvious here but it's the whole premise for my solution)

Therefore, in order to find our first digit, why not just find the biggest digit in the line?

Well, there is a small catch: can we use it as our first digit and still get the remaining digits?

The answer is pretty simple: how many digits are there left in the line after it? If there are enough, then yes and this is our best pick. Otherwise, we shouldn't even consider it to begin with.

Second question: what if there are two digits with the same value and they're both the biggest value?

Well, pick the first one, the second one can then be picked as the second digit :D.

Now that this is said, finding the second digit, third digit etc. is exactly the same,
we just crop the line by only keeping the digits on the right of the one we picked.

```hs
findJoltage :: Int -> String -> Int
findJoltage numBat = read . step numBat
    where step 0 _ = []
          step n battery = first : next
            where (first, idx) = maximumBy (comparing fst) . drop (n - 1) . reverse $ zip battery [1 .. ]
                  next = step (n - 1) . drop idx $ battery
```

Now, some explanations here:
- step is the iteration that I just described earlier. n is the number of digits we have to pick and battery is our line.
- because I never converted chars into ints I can simply use read on the constructed list of chars to get an int (because in Haskell [Char] is the same as String :D)
- getting the highest number with at least n digits after is done like this: I zip the digits with their indices, then I reverse this list and I drop (n - 1) numbers. I take the biggest number and it's index, which I use to crop my line for the next iteration.

You may be wondering: why do I reverse and drop instead of simply taking (length - n)?

This is pretty simple: maximumBy solves ties by taking the rightmost element. However, as stated earlier, we want the leftmost one. Reversing the list is a simple way of solving that.

## Conclusion

Have a good night.
