## Day 06

I woke up this morning. My friend sent me a message: "You can do today's puzzle using only maths"

So I did the maths:
![Maths](https://media.discordapp.net/attachments/455387472730259459/1182111075894829136/07-12-2023_106_AM_Microsoft_Lens.jpeg?ex=658381bb&is=65710cbb&hm=5892789189fac9348e4faef4fda7f730dfd340fff6c6cf574902a1081b772613&=&format=webp&width=794&height=1136)

If you're allergic to maths, basically remember this:
 - This can be solved using the quadratic formula
 - If t1 and t2 are the solutions of the quadratic equation, then the puzzle answer is ceil(t2 - 1) - floor(t1 + 1) + 1

```hs
import Data.Tuple.Extra
import System.Environment

type Input = [(Integer, Integer)]
type Output = Int

parseInput :: String -> Input
parseInput = uncurry zip . go . lines
    where go [t, d] = both (map read . tail . words) (t, d)

getBounds :: (Integer, Integer) -> (Int, Int)
getBounds (t, d) = (ceiling (t2 - 1), floor (t1 + 1))
    where delta      = t * t - 4 * d
          root       = sqrt(fromIntegral delta)
          (t1 , t2)  = ((fromIntegral t - root) / 2, (fromIntegral t + root) / 2)

partOne :: Input -> Output
partOne = product . map ((+ 1) . uncurry (-)) . map getBounds

partTwo :: Input -> Output
partTwo = partOne . (: []) . both (read . concatMap show) . unzip
```
And so this is what I implemented.

But let's look at how I did stuff:

```hs
type Input = [(Integer, Integer)]
type Output = Int

parseInput :: String -> Input
parseInput = uncurry zip . go . lines
    where go [t, d] = both (map read . tail . words) (t, d)
```

For the input I want a list of pair of integers (time, distance). I used Integers and not Int because I thought Int might be too small (it isn't).

I split my puzzle input into two lines (the time line and the distance line). For each line I split by spaces, remove the header, and convert the numbers into integers.
Once I have parsed both of my lines, I zip them together to form pairs of (time, distance). I use the uncurry method to make it so zip takes a pair of list as input instead of taking two lists.

```hs
getBounds :: (Integer, Integer) -> (Int, Int)
getBounds (t, d) = (ceiling (t2 - 1), floor (t1 + 1))
    where delta      = t * t - 4 * d
          root       = sqrt(fromIntegral delta)
          (t1 , t2)  = ((fromIntegral t - root) / 2, (fromIntegral t + root) / 2)
```

getBounds is just solving the quadratic equation as descibred in my picture at the begining of this write-up.
If you're allergic to maths, simply remember that, for a given pair of time and distance, it gives the upper and lower bound of time that can be used to press the button to win the race.

For example, if you at least need to press the button for 2ms and at most 5ms, getBound will yield (5, 2).

```hs
partOne :: Input -> Output
partOne = product . map ((+ 1) . uncurry (-)) . map getBounds
```

Then, for each race from my input, I find the bounds for pressing the button, and for each bound I do (upper - lower + 1).
I did some weird Haskell stuff with uncurry once again because i couldn't be bothered with writing a lambda like a normal person.

Then I multiply every result to get the answer.

Part two is simply calling part one again, but it transforms the input beforehand (because I couldn't simply remade the parser, that would have been too easy)
```hs
partTwo :: Input -> Output
partTwo = partOne . (: []) . both (read . concatMap show) . unzip
```

So I start by unzipping my Input to get back the list of times and the list of distances. For each list, I convert everything back to strings, I concatenate the strings together, and I convert them back to Integers. This gives me a single pair of Integers, that I put inside a list before giving it to part one.
