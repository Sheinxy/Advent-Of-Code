## Day 17

Welcome to the mandatory "this puzzle can be solved because the input has specific properties" day!

## The input

The input describe a computer with the following parts:
- A program represented by a list of integers (opcodes and operands)
- Registers A B and C

On top of that, the computer also has the following things:
- A program counter
- A standard output

```hs
data Computer = Computer { program :: Array Int Int, pc :: Int, regA :: Int, regB :: Int, regC :: Int, stdout :: [Int]} deriving Show
```

Parsing the file is just a matter of reading the different parts of the file:
```hs
parseInput :: String -> Input
parseInput input = Computer (parseProgLine p) 0 (parseRegLine a) (parseRegLine b) (parseRegLine c) []
    where [a, b, c, _, p] = lines input
          parseRegLine regLine = read num
            where _ : _ : [num] = words regLine
          parseProgLine progLine = listArray (0, length ops - 1) ops
            where _ : [opsStr] = words progLine
                  ops          = read $ "[" ++ opsStr ++ "]"
```

## Part 1

### The problem

We have a list of instructions that our computer can execute. We need to execute them and get the output

### The solution

I started by coding an helper function to get the value of a combo operand.

It returns a function that takes a computer and returns either a register or a constant value:

```hs
comboOperand :: Int -> Computer -> Int
comboOperand n | n <= 3 = const n
               | n == 7 = error "Invalid operand"
comboOperand 4 = regA
comboOperand 5 = regB
comboOperand 6 = regC
```

Next, I simply coded all the opcode functions by following their specification:
```hs
adv :: Computer -> Computer
adv computer = computer { regA = res, pc = pcounter + 1 }
    where pcounter = pc computer
          operand  = program computer ! pcounter
          n   = regA computer
          d   = comboOperand operand computer
          res = n `div` (2 ^ d)

bxl :: Computer -> Computer
bxl computer = computer { regB = res, pc = pcounter + 1 }
    where pcounter = pc computer
          operand  = program computer ! pcounter
          b   = regB computer
          res = b `xor` operand

bst :: Computer -> Computer
bst computer = computer { regB = res, pc = pcounter + 1 }
    where pcounter = pc computer
          operand  = program computer ! pcounter
          res      = comboOperand operand computer `mod` 8

jnz :: Computer -> Computer
jnz computer = computer { pc = pcounter' }
    where pcounter = pc computer
          operand  = program computer ! pcounter
          pcounter' | regA computer == 0 = pcounter + 1
                    | otherwise          = operand

bxc :: Computer -> Computer
bxc computer = computer { regB = res, pc = pcounter + 1 }
    where pcounter = pc computer
          b = regB computer
          c = regC computer
          res = b `xor` c

out :: Computer -> Computer
out computer = computer { stdout = res : stdout computer, pc = pcounter + 1}
    where pcounter = pc computer
          operand  = program computer ! pcounter
          res = comboOperand operand computer `mod` 8


bdv :: Computer -> Computer
bdv computer = computer { regB = res, pc = pcounter + 1 }
    where pcounter = pc computer
          operand  = program computer ! pcounter
          n   = regA computer
          d   = comboOperand operand computer
          res = n `div` (2 ^ d)

cdv :: Computer -> Computer
cdv computer = computer { regC = res, pc = pcounter + 1 }
    where pcounter = pc computer
          operand  = program computer ! pcounter
          n   = regA computer
          d   = comboOperand operand computer
          res = n `div` (2 ^ d)

getInstruction :: Int -> Computer -> Computer
getInstruction 0 = adv
getInstruction 1 = bxl
getInstruction 2 = bst
getInstruction 3 = jnz
getInstruction 4 = bxc
getInstruction 5 = out
getInstruction 6 = bdv
getInstruction 7 = cdv
```

Note here that the opcode function assumes the pc has already been move forward past the opcode itself (and is now pointing on the operand)

Also note that I'm putting numbers in my stdout by appending them at the beginning,
in order to get the actual stdout I simply need to reverse it.

Now, all there is to do is to run an instruction by:
- Getting the current opcode
- Getting the associated instruction function
- Running this function with a computer where the pc has been incremented by 1

```hs
runInstruction :: Computer -> Computer
runInstruction computer = instruction computer'
    where pcounter    = pc computer
          opcode      = program computer ! pcounter
          instruction = getInstruction opcode
          computer'   = computer { pc = pcounter + 1 }
```

Now we simply need to run instructions until the pc is beyond the program:

```hs
runInstructions :: Computer -> Computer
runInstructions = head . filter isDone . iterate runInstruction
    where isDone computer = pc computer > (snd . bounds) (program computer)
```

Part one is only a matter of running the instructions, getting the stdout (reversed), and putting it as a comma-separated string:
```hs
partOne :: Input -> String
partOne = intercalate "," . map show . reverse . stdout . runInstructions
```

## Part 2

### The problem

We need to find which initial register A value will result in printing the program itself.

### The solution

The only general solution is to bruteforce everything.

This obviously will not work, therefore let's look at my input to see if there is anything peculiar about it.

Here is some pseudo-C of what my input does:
```hs
int regB, regC = 0, 0;

do
{
    regB = regA & 0b111;
    regB = regB ^ 1;
    regC = regA >> regB;
    regB = rebB ^ 5;
    regB = regB ^ regC:
    printf("%d\n", regB & 0b111);
    regA = rebA >> 3;
} while (regA != 0);
```

Ok so let's analyse things here:
- We only have a single loop that stops regA is equal to 0.
- We shift regA by 3 bits during each loop
- The only parts that matter for regB and regC are the last three bits
- What matters for regA during a single loop iteration are the 10 last bits (or so, I'm not certain my count is right).

Furthermore:
- The last printed number WILL depend on the first 3 bits of A.

So, if we can find a 3-bit number that prints the last number of our program, we can then shift that number by 3 and lookup all the potential candidates for the next 3 bits.
A potential candidate is a number that prints the second last number of the program.

For each potential candidate we do the same:
- Shift A by three
- Find the potential candidates for that A
- Look up on these potential candidates

If we stumble onto a number that can not print the expected number, then this candidate is invalid and we therefore can go onto the next one.

Otherwise, if the chain of candidates lead to printing the whole program, we're done and we can return that number:

```hs
partTwo :: Input -> Int
partTwo computer = fromJust $ findValue target 0
    where target = reverse . elems $ program computer
          tryValue value = last . stdout $ runInstructions (computer { regA = value })
          findValue [] acc = Just acc
          findValue (t : ts) acc
                | null candidates = Nothing
                | otherwise       = head $ filter isJust [findValue ts candidate | candidate <- candidates]
            where values     = [acc * 2 ^ 3 .. acc * 2 ^ 3 + 7]
                  candidates = map fst . filter ((t ==) . snd) $ zip values (map tryValue values)
```

## The end

At least this input is easy to analyse. I'm not a fan of input-dependant days.
