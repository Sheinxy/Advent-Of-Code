## Day 25

Merry christmas everyone!

## Part 1: The actual writeup

As usual, day 25 is only composed of one, fairly easy, part.

### The problem

We have a bunch of keys and locks. We want to find the lock that don't overlap the
'#' of the keys.

### Step 1: Parsing the input

I parse my input as a tuple: one part represents the keys, the other the locks.

```hs
type Input = ([[Int]], [[Int]])
```

In order to get all of that, I start by splitting on each empty line,
and I get every schematic as a grid of characters.

I then partition them between keys and values by looking at their first row:
is it only made up of '#'.

```hs
    where schematics = partition (all (== '#') . head) . map lines . splitOn "\n\n" $ input
```

Then, I transform the schematics:
Instead of keeping them as a grid, I write them down as an array of Int.

For keys, each Int represents the number of '#' on each column,
for locks it represents the number of '.'.

This can be found the same way for both: by counting the number of same characters at the beginning.

```hs
parseInput :: String -> Input
parseInput input = both (map asColCount) schematics
    where schematics = partition (all (== '#') . head) . map lines . splitOn "\n\n" $ input
          asColCount = map (length . head . group) . transpose
```

### Step 2: Solving the problem

Now, I go through each possible pairing of key and locks, and I check if the lock
can fit into the key.

A lock can fit into a key if the number of '.' on each column is greater than or equal
to the number of '#' on the respective columns of the key.

```hs
partOne :: Input -> Output
partOne (keys, locks) = length [(key, lock) | key <- keys,
                                              lock <- locks,
                                              fits key lock]
    where fits key = and . zipWith (<=) key
```

## Part 2: The part where I talk about AOC.

Well before talking about AOC, I will introduce myself in case you don't know me.

Hello everyone, I'm Sheinxy, a 22 years old French computer science student.
This is my fifth year of doing the AOC, my fifth year doing it in Haskell,
my third year managing to solve all problems, and my second year writing writeups every day.

This year's AOC was very special to me. Obviously, this has been the tenth edition,
so it was already a special year, and this made me realised that I've been there for half of them.

But most importantly, this was (hopefully, unless I somehow flunk), my last year doing it
as a college student!

I started (got talk into doing it by [a good friend of mine](https://github.com/Adam-Alani)) doing
this four years ago, as I was just a first year undergraduate student.
And now, I'm on my last few months of school, this feels so weird to me!

And I have to say, this was a really great edition!

I'm not doing AOC competitively, so I usually take some time to read the story,
and revisiting places from the previous years made me feel somewhat nostalgic! 
(well, for the ones I know about at least, so starting from 2020).

I felt like the puzzles were a bit easier than last year, but that's fine,
there still were a few hard ones.

Unlike last year I don't have puzzles that I disliked, even the input-dependant ones
felt solvable without having to take wild guesses here.

So, I'm only going to talk about my three favorites (unlike last year where I also talked about my three less favorite):

### Top 3: Day 20

This day was very nice. It seemed hard to me at first,
so I started scribbling down ideas in my notebook and I found one that
could solve part 1 in basically no time.

Then solving part 2 was just a matter of understanding why my solution worked,
and adapting it to part 2.

Overall, I liked this puzzle because it is simple to understand, requires some level
of thinking to solve, but is still not hard.

### Top 2: Day 24

To be frank, I'm not sure if I want to put it second place or first place.

I had quite a blast with this puzzle!

Part one allowed me to do something that I wanted to do all year: Having some kind of "relexive-Haskell" solution!

Part two was quite challenging, but it felt so good investigating how things work and getting
an understanding of how to find what definitions are wrong and how to find the right definition.

I especially had a blast making the writeup for this one!

### Top 1: Day 21

Although it might have been a tiny bit too difficult, and gave me a few headaches (figuratively),
this is probably the day that I am the proudest to have solved.

This day had everything:
- A not-so-trivial explanation
- A use for my newly-discovered [Data.Function.Memoize](https://hackage.haskell.org/package/memoize-1.1.2/docs/Data-Function-Memoize.html) library
- Not input dependant

---

Now, let's talk about the future.

Usually, this is the part where I talk about how I don't know if I'll keep doing it in Haskell
next year.

Well not this time: I **will** do it in Haskell next year!

More importantly, I will try to prepare more than ever,
write down a small library of functions I usually have to recode every year,
improve my understanding of the language and my proficiency in it, etc etc.

Maybe I'll go for additionaly challenges:
- I'd like to go for the leaderboard one day, but I'm not really good at competitive programming, and people using LLMs to solve the puzzles kind of ruin the leaderboard in my opinion (like, on the one hand I find it interesting to see how they manage against the puzzles, however they shouldn't be on the leaderboard: you wouldn't bring a chess bot to a chess competition and say that you're number 1 when it wins for you)
- I'd really like to create visualisations of each puzzle. This seems a bit hard to do in Haskell, and it's some extra-work, but it would fit well with my writeups.

Yes, because I'll definitely keep making writeups! I like writing them!
Although, I think I should try to take more time when writing them, I usually write them in one go as soon as I'm done with the puzzle,
and I don't take much time to proof-read it (so basically all of them are in a draft state, which is why my wording is often weird, there are spelling mistakes everywhere etc etc.)

I'd also like to find a more polished way to publish them.
My github.io page is a bit outdated, and I'm not a fan of the default markdown rendering.

---

Are you still reading this?

Well if you are, I wanted to thank you. I don't know how many people read this writeups,
but every time someone tells me that they read my writeups it makes me really happy!

I'm having fun making these, and I hope you have fun reading them, getting a glimpse into
how my weird brain solves these puzzles!

And if these can get some people into trying Haskell, I'll keep doing these forever!


---

Thank you all who read these,
thanks to the people behind the AOC,
and a huge thanks to all my friends, those who've been doing the AOC with me and those who've supported through it.

See you next year (or maybe before, who knows, writeups are not reserved for AOC), and have a

# Merry Christmas! 🐈‍⬛🎅
