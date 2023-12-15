## Day 15

My morning routine usually consists of the following:
 - I wake up at 7AM (one hour after the puzzle is made available)
 - I look at the puzzle
 - I go back to sleep

This morning was different. I looked at the puzzle and thought (this is way too easy to be left for later).

So here is what I did at 7:15AM this morning (yeah I still had to take 15 minutes to process being awake):

```hs
data Lens = Lens { label :: String, focal :: Int } deriving (Show)
type Boxes = Map Int [Lens]

type Input = [String]
type Output = Int

parseInput :: String -> Input
parseInput = splitOn "," . head . lines

getHash :: String -> Int
getHash = foldl (\acc x -> (acc + ord x) * 17 `rem` 256) 0

putInBoxes :: Boxes -> String -> Boxes
putInBoxes boxes = go . span (`notElem` "=-")
    where go (lab, "-"    ) = adjust (filter ((/= lab) . label)) (getHash lab) boxes
          go (lab, '=' : n) | lab `elem` labels     = adjust (const $ before ++ [Lens lab (read n)] ++ after) hash boxes
                            | otherwise             = adjust (Lens lab (read n) :) hash boxes
                            where hash              = getHash lab
                                  elements          = boxes ! getHash lab
                                  labels            = map label elements
                                  (before, _:after) = span ((/= lab) . label) elements

partOne :: Input -> Output
partOne = sum . map getHash

partTwo :: Input -> Output
partTwo = sum . map getPower . toList . foldl putInBoxes (fromList [(i, []) | i <- [0 .. 255]])
    where getPower (i, xs) = sum [(i + 1) * j * focal lens | (j, lens) <- zip [1 .. ] $ reverse xs]
```

Now, let me explain everything 😸

## The input:

"The initialization sequence (your puzzle input) is a comma-separated list of steps to start the Lava Production Facility."

So let's just make a list of every comma separated value!

By the way, fun fact: here is what the puzzle says "Ignore newline characters when parsing the initialization sequence." Well I chose to ignore that as there is only one line anyway lol

```hs
type Input = [String]
type Output = Int

parseInput :: String -> Input
parseInput = splitOn "," . head . lines
```

So, I take the first (and only) line, and I chunk by commas. That is all for the input parsing!

## The HASH Algorithm:

Here is what the HASH algorithm does:
 - Start with a current value of 0
 - For every character in the string:
     - Determine the ASCII code for the current character of the string.
     - Increase the current value by the ASCII code you just determined.
     - Set the current value to itself multiplied by 17.
     - Set the current value to the remainder of dividing itself by 256

So... Let's just write that...

```hs
getHash :: String -> Int
getHash = foldl (\acc x -> (acc + ord x) * 17 `rem` 256) 0
```

Let's refresh our memory of what [foldl](https://hackage.haskell.org/package/base-4.18.0.0/docs/Data-List.html#v:foldl) does.

Basically, it takes three arguments:
 - A binary function (let's say f)
 - An initial value  (let's say v)
 - A list of value to iterate through (let's say [x1, x2, ...])

And it does f(f(f(v, x1), x2), x3) etc.

Basically, if you come from iterative programming languages, think of it this way:
```py
def fold(f, v, xs):
    acc = v
    for el in xs:
        acc = f(acc, el)
    return acc
```

So this is precisely what we want here: we want to apply some computation to every character of our string.

Our computation being: ((current value + ascii_code(current character)) * 17) % 256

So this is the function that we pass as our first argument.

Then the second argument is the initial value (0)

And the third argument (which is not named here because I am using [point-free style](https://wiki.haskell.org/Pointfree)) is our string!

##### (Yeah, I'm explaining things a lot here because I don't have much to say. Plus, I believe it is all interesting stuff! I am no professional when it comes to Haskell nor Functional Programming in general, but if these explanations might get people into trying Haskell then I'm happy 😸)

Now that we can get the has for a string, we simply need to get it for each step of our sequence, and sum all the results together:

```hs
partOne :: Input -> Output
partOne = sum . map getHash
```

## Putting things into boxes because mom said so:

Now, part 2 of the puzzle has quite a long explanation. (So long in fact that it took me 20 minutes to understand it :,D). So here is a shortened version:

- There are 256 boxes, numbered 0 through 255
- Each step in the sequence is of one of two forms:
    - Either XXX- (where XXX is one or more letters)
    - Or     XXX=N (where XXX is one or more letters and N is a number, the focal length of a lens labelled XXX)
- To know the box where the length labelled XXX should go in, simply take the hash of the label. XXX goes to the box numbered getHash ("XXX")
- If the step is XXX-, remove (if present) the lens labelled XXX from the corresponding box. The order of the other lenses doesn't change AAA=1 XXX=2 BBB=3 -> AAA=1 BBB=3
- If the step is XXX=n:
    - If the lens is not present in the box, add it at the end of the box: AAA=1 BBB=3 -> AAA=1 BBB=3 XXX=n
    - If it already in the box, simply change its focal length value: AAA=1 XXX=2 BBB=3 -> AAA=1 XXX=n BBB=3

Hopefully this is more digestable to read 👉👈

So now, what I did is that I made a function that took my boxes (represented as a Map from int to lenses) and a step in my sequence, and that performs the adequate box operation:

```hs
data Lens = Lens { label :: String, focal :: Int } deriving (Show)
type Boxes = Map Int [Lens]

putInBoxes :: Boxes -> String -> Boxes
putInBoxes boxes = go . span (`notElem` "=-")
    where go (lab, "-"    ) = adjust (filter ((/= lab) . label)) (getHash lab) boxes
          go (lab, '=' : n) | lab `elem` labels     = adjust (const $ before ++ [Lens lab (read n)] ++ after) hash boxes
                            | otherwise             = adjust (Lens lab (read n) :) hash boxes
                            where hash              = getHash lab
                                  elements          = boxes ! getHash lab
                                  labels            = map label elements
                                  (before, _:after) = span ((/= lab) . label) elements
```

I start with [span](https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-List.html#v:span) to divide my step into two parts:
 - The XXX part (which is the part where every element is not an element of the string "=-")
 - The - or =n part

And I apply the actual computations on that:
- If the second part is "-", then I simply need to remove every lens with the freshly-found label from the corresponding box.
- If the second part starts with '=', then there are two possibilities:
    - If the label is already in the box, I change the focal length value. To do that I don't actually change it, but I rather remake the list by concatenating the list of elements before that label with the new lens and the list of elements after that lens.
    - If the label is not in the box, I add it.


Now to solve the puzzle, I simply need to apply that to every step of my input sequence, starting with a list of 256 empty boxes that gets updated with each step... That's just calling a fold function once again!

After the computation is done, I simply need to get the power for each lens in each box, and sum them all together!

```hs
partTwo :: Input -> Output
partTwo = sum . map getPower . toList . foldl putInBoxes (fromList [(i, []) | i <- [0 .. 255]])
    where getPower (i, xs) = sum [(i + 1) * j * focal lens | (j, lens) <- zip [1 .. ] $ reverse xs]
```


That's all for today! 😸

(If you have anything you'd like to add, please don't hesitate telling me!)
