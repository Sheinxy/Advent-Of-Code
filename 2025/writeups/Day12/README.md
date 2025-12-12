# The Great Adventures of Sheinxy 6

## Chapter 12: The twelfth day of Christmas

It was a fair winter night. Good thing for me, as I hate cold winter nights.\
I sat down at my desk as I usually do, and I started reading today's puzzle.

It was about packing shapes inside rectangular regions. The input looked pretty
annoying to parse:

```
0:
###
##.
##.

1:
###
##.
.##

2:
.##
###
##.

3:
##.
###
##.

4:
###
#..
###

5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2
```

Fortunately, I was used to parsing weird inputs at that point.\
This was my sixth time participating, I was no longer a rookie and I knew just what to do.

Without much hesitation, I started to write the code. It was a bit complex,\
but not really hard. It started by getting the lines from the raw input,
before splitting on the empty lines.\
For the first part, it simply removed the index line, while the parsing for the second
part was somewhat more complicated.\
For each line of the second part, it started by splitting on colons. The left side was then split on the letter 'x' before converting to integers, while the second part was split by spaces before doing the same.\
All of these were then put inside fitting data structures.

```hs
type Present = [String]
data Region = Region { regionSize :: (Int, Int), regionPresents :: [Int] } deriving Show
data Input  = Input { presents :: [Present], regions :: [Region] } deriving Show

parseInput :: String -> Input
parseInput raw = Input { presents = shapes', regions = regions' }
    where input = splitOn [""] . lines $ raw
          (shapes, regions) = (init input, last input)
          shapes' = map tail shapes
          regions' = map parseRegion regions
          parseRegion r = let (regSize, ':' : regPres) = span ( /= ':') r
                          in Region (parseSize regSize) (map read . words $ regPres)
          parseSize s = let (a, 'x' : b) = span ( /= 'x') s
                        in (read a, read b)
```

"Good.", I thought to myself, "Now let's read what the puzzle is really about".

I continued reading the webpage. The problem was simple yet hard.\
I had to fill each region in the input with the required number of presents, indicated by the list.\
The presents fill the spaces marked by the character '#'. They can be rotated, flipped, and
two presents can fit together as long as the spaces marked by '#' don't overlap.

"Hmm.", I pondered, "The only solution I can think of would be to bruteforce it."

I didn't like that thought. I could already feel that the code would be horrible to write
and the runtime even worse.

"Maybe this kind of problem is quite famous.", I said. I opened my favorite search engine,
duckduckgo (I like the funny duck), and typed "Packing Problem".\
This led me to a [wikipedia page](https://en.wikipedia.org/wiki/Covering_problem)


"Goodness gracious!", I exclaimed, "This is a [NP-Complete](https://en.wikipedia.org/wiki/NP-complete) problem!"

If you don't know about NP-Complete problems, they are "yes/no" question were verifying a solution is fast, but
finding that solution is really slow with our current knowledge.

"What am I going to do...", I cried.

Suddenly, a voice emerged from the shadows, as if it were trying to guide me:

"Meow."

It was my cat.

"You're right, I shouldn't give up, not after all I've done, not after sacrificing so much for Santa!\
Let me think..."\
[I started lightly tapping my forehead with my hand.](https://www.youtube.com/watch?v=ABWOqs_cqZ8)\
"I know!", I shouted, "I'll just prove that [P=NP](https://en.wikipedia.org/wiki/P_versus_NP_problem)!\
If I do that, and I find a polynomial solution then Santa will be very happy!"

"Meow!", insisted the cat.

"You're right... That wouldn't be fair to all those people who spent their life trying to solve
that problem if I arrived out of nowhere and solved it for them...", I said with a sad voice,
"But what else can I do?"

"Meow...", she answered, leaving the room.\
*(Editor's note: the cat was actually trying to lead this insane person to her food bowl, which was half-full and therefore completely empty.)*

"You're right! Maybe I could start by filtering out the easy cases first!", I said.

"If I remove the cases that can trivially fit everything, and remove the ones that can obviously not fit everything, I'll hopefully be left with just a few cases that I can solve manually, or find an algorithm for just these few cases!"

I started working on filtering out the invalid regions.

The idea was pretty simple: if we try to fit more '#' inside a region than its size,
then this region is invalid.

```hs
isTriviallyInvalid :: [Present] -> Region -> Bool
isTriviallyInvalid ps reg = x * y < numPresents
    where numPresents = sum . zipWith (*) (regionPresents reg)
                      $ map (count (== '#') . concat) ps
          (x, y) = regionSize reg
```

"Let's see how many regions we have left...", I said.

```hs
partOne input = (length input, length withoutTriviallyInvalids, length input - length withoutTriviallyInvalids)
    where ps = presents input
          withoutTriviallyInvalids = filter (not . isTriviallyInvalid ps) . regions $ input
```

```
➜  Advent-Of-Code git:(main) ✗ cabal run AOC2025 12 one 2025/inputs/12
Day 12:
(1000,591,409)
    Part one: 14.29 ms
Total:
    14.95 ms
```

"Wow, that's almost half of them gone!", I shouted with hope filling my voice,\
"Now let's filter out the trivially valid... I'll count them back later."

Trivially valid regions were easy to find:\
A region is trivially valid if it has more 3x3 squares than we need.

Because each shape fits inside a 3x3 square, if we have more 3x3 squares in
the region than there are shapes to fit inside it, a simple solution would
be to put all the presents inside their individual 3x3 square, no overlapping needed!

```hs
isTriviallyValid :: Region -> Bool
isTriviallyValid reg = numBlocks <= x' * y'
    where (x, y) = regionSize reg
          (x', y') = (x `div` 3, y `div` 3)
          numBlocks = sum . regionPresents $ reg
```

"Let's see how many we have left now..."

```hs
partOne input = (numRegions, length unknowns, numRegions - length unknowns)
    where ps = presents input
          numRegions = length $ regions input
          withoutTriviallyInvalids = filter (not . isTriviallyInvalid ps) . regions $ input
          unknowns = filter (not . isTriviallyValid) $ withoutTriviallyInvalids
```

I pressed the "Enter" key, fearing what the program might yield.

The result was nothing I had ever expected:

```
➜  Advent-Of-Code git:(main) ✗ cabal run AOC2025 12 one 2025/inputs/12
Day 12:
(1000,0,1000)
    Part one: 14.74 ms
Total:
    15.66 ms
```

"Eric you little rascal! You did it again! Of course! I should have expected the input to just have trivial cases!"

I changed the code to only count them:

```hs
type Output = Int
isTriviallyValid :: Region -> Bool
isTriviallyValid reg = numBlocks <= x' * y'
    where (x, y) = regionSize reg
          (x', y') = (x `div` 3, y `div` 3)
          numBlocks = sum . regionPresents $ reg

partOne :: Input -> Output
partOne = count isTriviallyValid . regions
```

I submitted the answer.

```
➜  Advent-Of-Code git:(main) ✗ cabal run AOC2025 12 one 2025/inputs/12
Day 12:
Sent: 591
     That's the right answer!  You are <span class="day-success">one gold star</span> closer to decorating the North Pole. <a href="/2025/day/12#part2">[Continue to Part Two]</a>
    Part sone: 553.9 ms
Total:
    554.8 ms
```

I was overwhelmed with joy. I had done it once again, I completed the Advent Of Code in Haskell.

I closed the lid of my MacBook, knowing that, somewhere, Santa was proud of me.

The cat jumped onto the closed computer lid to lay down. I gave it some pets while wondering about what to do next.

"Until next year...", I said, softly.

The End.

---

Heya! 🐈‍⬛ 

Are you still reading? Sorry for that weird writeup, I just thought it would be funny to
end on something like this, especially with such a weird last puzzle! :p

Anyway, this is the part where I talk about my experience with this year's AoC,
so if you were only here to see me explain my solutions then you can leave I guess
(I don't even know if that many people read those anyways, but if you do thanks for reading it <3)

### This year was different

So, this year was quite different from usual eh? Only 12 days.

I don't mind that much, though I feel like we've kind of lost the "Advent" part of the "Advent of Code".

Maybe releasing puzzles every other day would have been better? I don't know.

Anyway, that aside, this year was nice!

### The puzzles

I didn't find any puzzle particularly hard. I found most of them ranging from easy to medium difficulty.

I won't complain though! I do AoC on my free time, so not losing that much sleep on puzzles is nice :)
And although I didn't find them that difficult, they still were really nice to solve, I really had an enjoyable time! Thank you mister Wastl for all your work, I've been doing this since 2020 and I would definitely not
be the Haskell enjoyer that I am without the AoC!

### Haskell

Talking about Haskell, I still love the language, but I feel like I've came to a point where
I still have a lot to learn, and the way I do the AoC doesn't really let me learn much more new things.\
Don't get me wrong, I still learn a few things here and there every year, but I really want to go
deeper into things like [monads](https://wiki.haskell.org/Monad) and all, and I don't really
take time to do that in AoC (heck, I've never even used [Parsec](https://en.wikipedia.org/wiki/Parsec_%28parser%29)
which is like THE parser combinator library in Haskell.

Still, I'm quite proud of what I've done this year, and I'm especially happy with my new setup for AoC!

Not only is it nice to work with, not only can I submit my answer in one command (instead of copy pasting it),
I also get a nice recap of my solution!

```
➜  Advent-Of-Code git:(main) ✗ cabal run AOC2025
Day 01:
1165
    Part one: 8.690 ms
6496
    Part two: 3.886 ms
Day 02:
9188031749
    Part one: 56.40 ms
11323661261
    Part two: 38.20 ms
Day 03:
17376
    Part one: 1.906 ms
172119830406258
    Part two: 4.836 ms
Day 04:
1518
    Part one: 14.88 ms
8665
    Part two: 232.8 ms
Day 05:
635
    Part one: 6.636 ms
369761800782619
    Part two: 1.489 ms
Day 06:
5524274308182
    Part one: 6.817 ms
8843673199391
    Part two: 3.765 ms
Day 07:
1711
    Part one: 37.38 ms
36706966158365
    Part two: 19.37 ms
Day 08:
75582
    Part one: 122.3 ms
59039696
    Part two: 282.4 ms
Day 09:
4786902990
    Part one: 6.116 ms
1571016172
    Part two: 731.3 ms
Day 10:
449
    Part one: 60.27 ms
17848
    Part two: 5.175 s
Day 11:
590
    Part one: 1.930 ms
319473830844560
    Part two: 2.776 ms
Day 12:
591
    Part one: 12.89 ms
"Merry Christmas! <3"
    Part two: 10.13 μs
Total:
    6.838 s
```

### The puzzles, again

I usually do a little top 3 of my favorite puzzles. This year should
be no exception I suppose...

But I really don't know how to rank them to be fair...

Instead, here is my top 3 silliest solutions:

Number 3: Day 5 part 2

```hs
partTwo :: Input -> Output
partTwo = RSet.size . RSet.fromRangeList . fst
```

I just said "from library import solution" and I was done lol.

Number 2: Day 8

```hs
-- A data structure representing the connected components of an
-- undirected graph, without storing any of the graph’s edges.
data ComponentMap k = ComponentMap
  { idMap   :: M.Map k Int
  , sizeMap :: M.Map Int Int
  } deriving (Show, Eq)

mergeComponents :: Ord k => ComponentMap k -> Int -> Int -> ComponentMap k
mergeComponents (ComponentMap mIds mSize) id1 id2 = ComponentMap mIds' mSize'
    where (idMerge, idVoid) =  (min id1 id2, max id1 id2)
          mIds'  = M.map (\x -> if x == idVoid then idMerge else x) mIds
          mSize' = M.insert idVoid 0
                 . M.adjust (\x -> x + (mSize ! idVoid)) idMerge
                 $ mSize

insertEdge :: Ord k => ComponentMap k -> (k, k) -> ComponentMap k
insertEdge cMap@(ComponentMap mIds mSize) (a, b) =
    case (mIds !? a, mIds !? b) of
        (Nothing, Nothing) ->
            let newID  = M.size mSize
                mIds' = foldl (\m p -> M.insert p newID m) mIds [a, b]
                mSize' = M.insert newID 2 mSize
            in ComponentMap mIds' mSize'
        (Just idVal, Nothing) ->
            ComponentMap
                (M.insert b idVal mIds)
                (M.adjust (+ 1) idVal mSize)
        (Nothing, Just idVal) ->
            ComponentMap
                (M.insert a idVal mIds)
                (M.adjust (+ 1) idVal mSize)
        (Just id1, Just id2)
            | id1 == id2 -> cMap
            | otherwise -> mergeComponents cMap id1 id2
```

That wasn't just silly, that was overkill.

Number 1: Day 2, the silliest day

```hs
-- https://oeis.org/A004216
-- a(n) = floor(log_10(n))
-- That's just getting the number of digits in a number
a004216 :: Integer -> Integer
a004216 n = if n <= 9 then 0 else 1 + a004216 (n `div` 10)

-- https://oeis.org/A020338
-- a(n) = n*10^(A004216(n)+1) + n
-- This is just repeating n twice :D
a020338 :: Integer -> Integer
a020338 n = n * 10 ^ (a004216 n + 1) + n

partOne :: Input -> Output
partOne input = sum . filter isInRange . map a020338 $ [1 .. 99999]
    where isInRange n = any (\(a, b) -> a <= n && n <= b) input

-- https://oeis.org/A239019
-- Numbers which are not primitive words over the alphabet {0,...,9} (when written in base 10).
-- d is the length of the numbers
a239019  :: Integer -> [Integer]
a239019 d = S.toList r'
  where r = S.fromList [x * (10 ^ d - 1) `div` 9 | x <- [1 .. 9]]
        r' = foldl step r . filter (/= d) $ primeFactors d
        step acc p = S.union acc (S.fromList [x * (10 ^ d - 1) `div` (10 ^ q - 1) | x <- [10 ^ (q - 1) .. 10 ^ q - 1]])
            where q = d `div` p

partTwo :: Input -> Output
partTwo input = sum . concatMap (filter isInRange . a239019) $ [2 .. 10]
    where isInRange n = any (\(a, b) -> a <= n && n <= b) input
```

You know, most people check the numbers inside the ranges, not the numbers AGAINST the ranges.

Somehow this was not too slow :)

### And now, what next?

This is a question that I ask myself every year. I spend almost a full month
with my mind thinking about Haskell almost all the time (this is hyperbolic), and when it's over
I don't know what to do anymore.

This year is a bit weird though, I only spent 12 days coding in Haskell, leaving
some kind of weird feeling of being unfulfilled.

I need to do more Haskell >:3

Now, I don't know what I'll do next. Maybe I'll come back to do some funky visualisations (in Haskell!),
so stay tuned for that (I'll make a post on my bluesky account if I ever do that).

I should also probably get back to coding my NES emulator (it've barely made any progress on it and
it's a total mess because I have no idea what I'm doing), that could be fun!


Anyway, I don't really know what I'll do next, all I know is that I'll keep doing Haskell for fun,
forever >:3


---

Are you still reading? Thank you very much 🐈‍⬛ ❤️ 
