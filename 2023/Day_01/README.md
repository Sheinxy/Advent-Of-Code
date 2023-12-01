## Day 01

Let's talk a little bit about me first, shall we?

This is my fourth time doing this in Haskell. Last year was the first time
I managed to solve all days, so I thought I might stop doing thoses in Haskell
as I felt like I had accomplished everything I ought to do with this language.

But after thinking for a while, looking for a challenge that would make me happy,
I realised that this small period of the year where I do Haskell is probably my favorite time
of the year. Thus, I've decided once again to solve thoses in Haskell!

TL;DR: Hello, my name is Sheinxy and I'm that weird Haskell guy! 🐈‍⬛

Anyway, let's start with the challenge. For this year, I've decided to make a small
shell script to generate some boilerplate code to make things faster (let's see
how long I use this until I find it not specific enough for each problem):

```hs
module Main where

import Data.Char
import Data.List
import System.Environment

type Input = [String]
type Output = Int

parseInput :: String -> Input
parseInput = lines

partOne :: Input -> Output
partOne = sum . map (\s -> read $ [head s, last s] ) . map (filter isDigit)

partTwo :: Input -> Output
partTwo = partOne . map go
    where go "" = ""
          go line@(c : xs)
            | "one"   `isPrefixOf` line = '1' : go xs
            | "two"   `isPrefixOf` line = '2' : go xs
            | "three" `isPrefixOf` line = '3' : go xs
            | "four"  `isPrefixOf` line = '4' : go xs
            | "five"  `isPrefixOf` line = '5' : go xs
            | "six"   `isPrefixOf` line = '6' : go xs
            | "seven" `isPrefixOf` line = '7' : go xs
            | "eight" `isPrefixOf` line = '8' : go xs
            | "nine"  `isPrefixOf` line = '9' : go xs
            | otherwise                 =  c  : go xs

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> (readFile $ last args)
    mapM (compute input)  $ init args
```

Okay, that makes the code quite long actually, so from now on I'm only going to focus
on the parseInput, partOne and partTwo functions.

```hs
type Input = [String]
type Output = Int

parseInput :: String -> Input
parseInput = lines

partOne :: Input -> Output
partOne = sum . map (\s -> read $ [head s, last s] ) . map (filter isDigit)

partTwo :: Input -> Output
partTwo = partOne . map go
    where go "" = ""
          go line@(c : xs)
            | "one"   `isPrefixOf` line = '1' : go xs
            | "two"   `isPrefixOf` line = '2' : go xs
            | "three" `isPrefixOf` line = '3' : go xs
            | "four"  `isPrefixOf` line = '4' : go xs
            | "five"  `isPrefixOf` line = '5' : go xs
            | "six"   `isPrefixOf` line = '6' : go xs
            | "seven" `isPrefixOf` line = '7' : go xs
            | "eight" `isPrefixOf` line = '8' : go xs
            | "nine"  `isPrefixOf` line = '9' : go xs
            | otherwise                 =  c  : go xs
```

Okay that is already less scary :3c

Parsing the input is just spliting the file line by line, nothing too fancy.
My input is therefore a list of lines, and my output is a number.


Let's focus on part one first:
```hs
partOne = sum . map (\s -> read $ [head s, last s] ) . map (filter isDigit)
```

I start by removing anything that is not a digit from the line, then I keep the first and last digit and convert them to an int.
I sum every line result together, and I'm done! That is not the most optimised (nor probably the most elegant), but it's simple enough in my opinion.

Then, part two:
```hs
partTwo = partOne . map go
    where go "" = ""
          go line@(c : xs)
            | "one"   `isPrefixOf` line = '1' : go xs
            | "two"   `isPrefixOf` line = '2' : go xs
            | "three" `isPrefixOf` line = '3' : go xs
            | "four"  `isPrefixOf` line = '4' : go xs
            | "five"  `isPrefixOf` line = '5' : go xs
            | "six"   `isPrefixOf` line = '6' : go xs
            | "seven" `isPrefixOf` line = '7' : go xs
            | "eight" `isPrefixOf` line = '8' : go xs
            | "nine"  `isPrefixOf` line = '9' : go xs
            | otherwise                 =  c  : go xs
```

Basically part two starts by "reparsing" the input before doing exactly what part one does.
The reparsing simply changes every number written in letter form as the same number in digit form.

The only small problem that I had with part 2 was that "oneight" is actually "18" and not "1ight",
so I cannot drop the whole "one" from the string (I was too lazy to check what can be a prefix of what to be optimise, so I simply drop the first letter)

That's all for today folks!
