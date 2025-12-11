## Day 10

xor.

## The Input

The input is written in a very annoying format to parse. It's made out
of three parts:
- The first part is a list of states, either . or #, written between square brackets.
- The second part is a list of list of numbers. These sublists are written between parenthesis.
- The last part is a list of numbers written between curly brackets.


My parsing is pretty rudimentary here, I'm just splitting the string where I need
it, and I rely a lot on Haskell's read function to parse the lists of numbers.

The only really crazy thing I do is that I parse the first list of states as a binary number
written in little endian. You'll see why later :)

```hs
data Instruction = Instruction { light :: Int, buttons :: [[Int]], joltage :: [Int] } deriving Show

type Input = [Instruction]

parseLine :: String -> Instruction
parseLine raw = Instruction lights buttons joltages
    where (lightRaw, ' ' : rest) = break (== ' ') raw
          (buttonsRaw, rest') = break (== '{') rest
          buttons = map (\x -> read $ "[" ++ init (tail x) ++ "]") $ words buttonsRaw
          joltages = read $ "[" ++ init (tail rest') ++ "]"
          lights = foldr (\x acc -> 2 * acc + if x == '.' then 0 else 1) 0 (init $ tail lightRaw)
```

## Part One: later

### The Problem

We want to find the least number of button presses we can do to go from the initial states (no lights on)
to the target state (part one of the input).

### The Solution

There are two states. Pressing a button flips the states.

Do you know what operation works well on a binary system and flips the state? xor :)

Consider one bit:
- 0 = off
- 1 = on

if we xor it with 1:
- off xor 1 = 0 xor 1 = 1 = on
- on xor 1 = 1 xor 1 = 0 = off

Flipping a switch is just xoring with one :))))

Our buttons can just be converted to binary numbers, for example pressing switch 0 and 2 is
like xoring with 0b101!

Another nice thing to know about xors: They're commutative, so the order doesn't matter,
and xoring twice with the same value is a null operation.

Why is that nice you ask? Well it means two things:
- As long as we find a set of button presses that yields the target we're fine, we don't have to care about the order
- No button should be pressed more than once.

Now, why don't we just bruteforce this? After all, there aren't that many buttons.

Let's start by getting all the possible combinations of button presses. Such a set
is called a [power set](https://en.wikipedia.org/wiki/Power_set). If we have three buttons (a, b, c), we could
- Press no button \{\}
- Press 1 button \{\{a\}, \{b\}, \{c\}\}
- Press 2 buttons \{\{a, b\}, \{b, c\}, \{a, c\}\}
- Press 3 buttons \{\{a, b, c\}\}

This is the powerset! \{\{\}, \{a\}, \{b\}, \{c\}, \{a, b\}, \{b, c\}, \{a, c\}, \{a, b, c\}\}

It's at most 2^n elements, so I think we should have at most 2^16 subsets to check for a line AT WORST. Fast enough :)

```hs
combinations :: [a] -> [[a]]
combinations = sortOn length . go
    where go []       = [[]]
          go (x : xs) = go xs ++ map (x : ) (go xs)
```

To know if a combination is valid, we just xor all the elements and check if it creates the target.

We can stop at the first one we find (which is the smallest because our powerset is ordered by length)
```hs
fewestPresses :: Instruction -> Int
fewestPresses inst = case solution of
                        Nothing -> error "Something went wrong. Check your input."
                        Just xs -> length xs
    where target  = light inst
          presses = combinations
                  $ map (\x -> sum [2 ^ n | n <- x])
                  $ buttons inst
          solution = find ((== target) . foldl xor 0) $ tail presses

partOne :: Input -> Output
partOne = sum . map fewestPresses
```

## Part Two: Stop thinking

### The Problem

Buttons should be pressed multiple times :)
In fact, the third part of the input indicates how many times a switch must be split.

### The Solution

At first, I tried to bruteforce it again. I just generated all the possible combinations of buttons that are still valid (won't detail
how I did it here, I don't remember :)).

This used a lot of RAM (I went up to 30GB of RAM used on my 16GB of RAM MacBook lol), and it was slow.

Instead, I started writing down the problem in a more mathematical form:

Let's focus on a single light for now, say light 0. Say it must be flipped 10 times.

Then, looking at the buttons, we know buttons 1, 3 and 6 flip light 0.

Let's call ti the number of times button i was flipped.

Therefore, we must have t1 + t3 + t6 = 10.

If we apply the same reasoning for the whole joltage part (and if we keep it more generic), we notice
that this is just a linear system of equations, one which can have multiple solutions :)

I'll be honest, I think I can solve one manually. I'm not sure if I can write a generic solver for that manually. I'm sure I don't want to.

Instead, I can use a solver which can minimize the solution and let it do all the heavy lifting for me!

Introducing ([once again](https://github.com/Sheinxy/Advent-Of-Code/blob/main/2023/Day_24/Day_24.hs)) [SBV](https://hackage.haskell.org/package/sbv)

In order to solve this problem, I start by creating all the `ti` variables, one for each button. They represent then number of times a button has been pressed.

Then, I add the constraint that these values must be at least 0.

Then, I add the constraints about the joltages:
- for a given joltage value for a given light, the sum of button presses for the buttons that flip that light must be equal to that joltage.

Finally, I want to minimize the total sum.

```hs
optimizePresses :: Instruction -> IO SMTResult
optimizePresses inst = optLexicographic $ do
    ts <- sIntegers ["t" ++ show n | n <- [0 .. length (buttons inst) - 1]]
    for_ ts $ \t -> do
        constrain $ t .>= 0
    for_ (zip [0 ..] (joltage inst)) $ \(i, jolt) -> do
       constrain $ fromIntegral jolt .== sum [t | (t, b) <- zip ts (buttons inst), i `elem` b]
    minimize "total" (sum ts)
```

I call this on all of my input value, retrieve the `total` values and sum them  all together.

(I'm pretty sure my do in my for is useless, but I like it :))

```hs
partTwo :: Input -> IO Output
partTwo input = do
    opts <- mapM optimizePresses input
    mins <- for opts $ \res -> do
                let x = fromJust $ getModelValue "total" res :: Int64
                return $ fromIntegral x
    return $ sum mins
```
