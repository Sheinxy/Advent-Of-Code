## Day 03

Regex goes brrrrrrrrrrrrrrrrrrr

```hs
import Text.Regex.TDFA
```

## The input:

I did not parse the input.

![Cat with sunglasses](https://www.pngitem.com/pimgs/m/370-3708742_memes-cat-sunglasses-cat-meme-hd-png-download.png)

It's way easier to work with the raw input here!

## Part 1:

### The problem:

We have an input that looks like this:
```
xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
```

We want to extract everything that has the form "mul(X,Y)", where X and Y are 
integers.

Once we've extracted that, we compute all the X * Y and we sum the results.

### The solution:

This just begs me to use a regex :)

The following regex will only match "mul(X,Y)", and put the numbers in matched groups:
```
mul\(([0-9]+),([0-9]+)\)
```

Once I've retrieved the matches, I simply convert each x and y to integers,
I multiply them together, and I sum all the results:

```hs
partOne :: Input -> Output 
partOne =  sum . map mul . (=~ "mul\\(([0-9]+),([0-9]+)\\)")
    where mul [_, x, y] = read x * read y
```

## Part 2:

### The problem:

Now there are "do()" and "don't()" that enable and disable the multiplication:
```
xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
```

### The solution:

I simply improved my regex to catch them as well:
```
do\(\)|don't\(\)|mul\(([0-9]+),([0-9]+)\)
```

Once that is done, I have a runner function that keeps the total sum as well
as the current state of multiplication as its accumulator.

When I encounter a "do()", I set the state to True, when I encounter a "don't()"
I set it to False.

When I encounter a mul, if the state is False I ignore, otherwise I add x * y to the
total sum.

```hs
partTwo :: Input -> Output
partTwo = snd .
         foldl run (True, 0) .
         ((=~ "do\\(\\)|don't\\(\\)|mul\\(([0-9]+),([0-9]+)\\)") :: String -> [[String]])
    where run (_, acc) ["do()", _, _] = (True, acc)
          run (_, acc) ["don't()", _, _] = (False, acc)
          run (False, acc) _ = (False, acc)
          run (True, acc) [_, x, y] = (True, acc + read x * read y)
```

## The end:

This was a very easy puzzle thanks to regexes.

Doing it without using them would've beem a bit more annoying I believe.
