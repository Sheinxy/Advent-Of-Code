## Day 05

This day was deceivingly simple. It looks hard at first, but when you take
some time to write things down on paper, everything flows nicely!

Also: never forget to read the input before working (THE NUMBERS ARE TOO LARGE TO PUT EVERY NUMBER INSIDE A MAP...)

Let's have a look at what I did:
```hs
import Data.List
import Data.List.Split
import System.Environment

data Map = Map { start :: Int, end :: Int, diff :: Int } deriving (Show)

type SourceMap = [Map]

type Input = ([Int], [SourceMap])
type Output = Int

parseInput :: String -> Input
parseInput input = (seeds, maps)
    where [seedl] : mapsl        = map lines . splitOn "\n\n" $ input
          seeds                  = map read  . words . drop (length "seeds: ") $ seedl
          maps                   = map (map (getMaps . map read . words) . tail) mapsl
          getMaps [dsr, srs, rl] = Map srs (srs + rl) (dsr - srs)

findMap :: Int -> SourceMap -> Int
findMap source smap | not . null $ lookup = source + (diff . head $ lookup)
                    | otherwise           = source
                    where lookup = filter (\(Map s e _) -> s <= source && source < e) smap

partOne :: Input -> Output
partOne (seeds, maps) = minimum . map (\x -> foldl findMap x maps) $ seeds

findMapWithRange :: SourceMap -> (Int, Int) -> [(Int, Int)]
findMapWithRange smap (source, stop)
    | isInInterval   = [(source + inDiff, stop + inDiff)] -- Range is in an interval,new range is linear application
    | spansIntervals = concatMap (findMapWithRange smap) newIntervals -- Range spans some intervals, we apply on the subintervals
    | otherwise      = [(source, stop)] -- Range has no part in common with an interval, so it doesn't change
        where inLookup = filter (\(Map s e _) -> s <= source && stop <= e) smap    -- Case 0: The range is inside an interval
              inDiff   = diff . head $ inLookup
              -- Looking for ranges covered by the seeds
              covRanges      = filter (\(Map s e _) -> (source < s  && e < stop)   ||   -- Case 1: Interval is fully in range
                                                       (s <= source && source < e) ||   -- Case 2: Start of range is in interval
                                                       (s < stop    && stop <= e)) smap -- Case 3: End of range is in interval
              bounds         = sort . ([source, stop] ++) . filter (\n -> source < n && n < stop) . concatMap (\m -> [start m, end m]) $ covRanges
              newIntervals   = zip bounds (tail bounds) -- Subintervals spanning (source, stop(
              -- Aliases to describe the two cases:
              isInInterval   = not . null $ inLookup  -- The range is fully in an interval
              spansIntervals = not . null $ covRanges -- The range is partially in an interval

partTwo :: Input -> Output
partTwo (seeds, maps) = minimum . map fst . concatMap go $ chunksOf 2 seeds
    where go [s, r] = foldl (\x m -> concatMap (findMapWithRange m) x) [(s, s + r)] maps
```

ALright, it's quite a long solution, but worry not I will explain everything!

Let's start with our input:
```hs
data Map = Map { start :: Int, end :: Int, diff :: Int } deriving (Show)

type SourceMap = [Map]

type Input = ([Int], [SourceMap])
type Output = Int

parseInput :: String -> Input
parseInput input = (seeds, maps)
    where [seedl] : mapsl        = map lines . splitOn "\n\n" $ input
          seeds                  = map read  . words . drop (length "seeds: ") $ seedl
          maps                   = map (map (getMaps . map read . words) . tail) mapsl
          getMaps [dsr, srs, rl] = Map srs (srs + rl) (dsr - srs)
```
I have a data structure called map which describes a mapping on a range using the start of the range,
the end of the range, and the transformation to apply (basically the number to add to get a new number).

I describe the type SourceMap as a list of that data structure. Thinking back, it may make more sense to swap the names for those two but anyway (I suck at naming stuff).

Our input is a tuple composed of a list of seeds, and a list of sourcemaps.

To get these, I split the puzzle input on every sequence of two linebreaks. The first sequence is the list of seeds, and the other ones are the sourcemaps.
To get the list of seeds, I just need to read all the numbers after the "seeds:" header.
To get the list of maps I simply need to get rid of the header line for each block, convert every number into an actual Int, and then simply put properly the data into my data structure.
To put them into my data structure I take the destination, source and range length and I compute the end of the range by adding the range length.

Not that hard.

Now, part one:
```hs
findMap :: Int -> SourceMap -> Int
findMap source smap | not . null $ lookup = source + (diff . head $ lookup)
                    | otherwise           = source
                    where lookup = filter (\(Map s e _) -> s <= source && source < e) smap

partOne :: Input -> Output
partOne (seeds, maps) = minimum . map (\x -> foldl findMap x maps) $ seeds
```

This is still the easy part:

Let's say that I want to know what the source 42 gives inside my maps, I simply filter my list of maps to get the ones
(and in fact the one, there is no overlap between 2 maps in the input) that contain the source inside its range.

If there is a map satisfying that condition, then I simply add the diff number (which was destination seed - source seed if you recall corectly) to get the destination number.
If no map matches that condition, then the number stays the same.

To get my result:
    - For a given seed, I apply the findMap function described above to get the new value of that seed, and I apply the findMap function to that new value etc. until I have applied it to every map.
    - I do that for all seeds
    - I keep the minimum result


Now part 2 is where things get... interesting...
```hs
findMapWithRange :: SourceMap -> (Int, Int) -> [(Int, Int)]
findMapWithRange smap (source, stop)
    | isInInterval   = [(source + inDiff, stop + inDiff)] -- Range is in an interval,new range is linear application
    | spansIntervals = concatMap (findMapWithRange smap) newIntervals -- Range spans some intervals, we apply on the subintervals
    | otherwise      = [(source, stop)] -- Range has no part in common with an interval, so it doesn't change
        where inLookup = filter (\(Map s e _) -> s <= source && stop <= e) smap    -- Case 0: The range is inside an interval
              inDiff   = diff . head $ inLookup
              -- Looking for ranges covered by the seeds
              covRanges      = filter (\(Map s e _) -> (source < s  && e < stop)   ||   -- Case 1: Interval is fully in range
                                                       (s <= source && source < e) ||   -- Case 2: Start of range is in interval
                                                       (s < stop    && stop <= e)) smap -- Case 3: End of range is in interval
              bounds         = sort . ([source, stop] ++) . filter (\n -> source < n && n < stop) . concatMap (\m -> [start m, end m]) $ covRanges
              newIntervals   = zip bounds (tail bounds) -- Subintervals spanning (source, stop(
              -- Aliases to describe the two cases:
              isInInterval   = not . null $ inLookup  -- The range is fully in an interval
              spansIntervals = not . null $ covRanges -- The range is partially in an interval

partTwo :: Input -> Output
partTwo (seeds, maps) = minimum . map fst . concatMap go $ chunksOf 2 seeds
    where go [s, r] = foldl (\x m -> concatMap (findMapWithRange m) x) [(s, s + r)] maps
```

I am going to go into more details later, but for now remember that findMapWithRange is a function that takes a sourcemap, a seed range, 
and returns a list of new seed ranges given by the input seed range.

I will start by first describing partTwo as it is easier:
    - I get all my seed ranges by chunking my seed list into chunks of two.
    - For each seed range I get the ranges of output locations using the go function that I will describe later.
      Once I have all the ranges, I concat them all into a single list of ranges as I do not care much about which seed range gave which location range
    - For each location range I get the start of the range as it will always be the minimum value for a range, and to get the puzzle answer I find the minimum of those starts.

The go function is a bit similar to what the partOne function did:
    - For a seed range of the form (start, end) (and not of the form (start, length)), I apply the findMapWithRange function to the first map,
      which gives me a list of new ranges.
    - With that list of new ranges (and with subsequent new lists in fact. In fact, you should notice that I have always been working with a list of ranges [(s, s + r)]), I apply the findMapWithRange to each range in that list, and I concat all of the list results together inside one list of new ranges. I do that until I have applied the new ranges to the last map.



NOW FOR THE HARD PART: findMapWithRange

There are two easy cases:
    - ![Default case](https://cdn.discordapp.com/attachments/455387472730259459/1182100573349228635/image.png?ex=658377f3&is=657102f3&hm=325db2e110f7b844a29f5ffc160c15046ac9d6acdf50ab09189a49f6030ad478&) The case where there is no map interval in our sourcemap that intersects our range. In that case, the range yields only itself as a result (as every number in the range with be mapped to itself). This case is so simple that we shall consider it our default case.
    - ![Fully mapped](https://cdn.discordapp.com/attachments/455387472730259459/1182100024243535943/image.png?ex=65837770&is=65710270&hm=a856c7e50d22387b77742d87731e5eeec1867a6ab37fcbda2aa87d83b7e4e2c1&) The case where the range is fully mapped by a single map interval in our sourcemap. In that case, the range yields a single new range because the transformation is a single addition applied to every number in that range.

Then, there is the third, hard case. That is when the range either intersects or even fully contains (while not fully equaling) a map interval in the sourcemap.

To handle this, one thing we can do is split our range into smaller subranges (green and purple on the really bad drawing below) that all fall inside one of the two easy cases:
![Sorry for the bad drawing, I made this on a trackpad](https://media.discordapp.net/attachments/455387472730259459/1182102336273915964/image.png?ex=65837997&is=65710497&hm=f7768994db5005e36841e01bf3330dad1803bf01df17102e18c3178e1f430acf&=&format=webp&quality=lossless&width=2160&height=1120)

If the map interval is fully contained inside the range, then the bounds of this interval will be in the new subintervals. If part of the interval is inside the range then only the part that is inside will be in our subintervals.

Translating that all into code:
```hs
findMapWithRange :: SourceMap -> (Int, Int) -> [(Int, Int)]
findMapWithRange smap (source, stop)
    | isInInterval   = [(source + inDiff, stop + inDiff)] -- Range is in an interval,new range is linear application
    | spansIntervals = concatMap (findMapWithRange smap) newIntervals -- Range spans some intervals, we apply on the subintervals
    | otherwise      = [(source, stop)] -- Range has no part in common with an interval, so it doesn't change
        where inLookup = filter (\(Map s e _) -> s <= source && stop <= e) smap    -- Case 0: The range is inside an interval
              inDiff   = diff . head $ inLookup
              -- Looking for ranges covered by the seeds
              covRanges      = filter (\(Map s e _) -> (source < s  && e < stop)   ||   -- Case 1: Interval is fully in range
                                                       (s <= source && source < e) ||   -- Case 2: Start of range is in interval
                                                       (s < stop    && stop <= e)) smap -- Case 3: End of range is in interval
              bounds         = sort . ([source, stop] ++) . filter (\n -> source < n && n < stop) . concatMap (\m -> [start m, end m]) $ covRanges
              newIntervals   = zip bounds (tail bounds) -- Subintervals spanning (source, stop(
              -- Aliases to describe the two cases:
              isInInterval   = not . null $ inLookup  -- The range is fully in an interval
              spansIntervals = not . null $ covRanges -- The range is partially in an interval
```
The otherwise case is our default case, the isInInterval is our fully mapped case.

Checking that the range is in our map is done similarly to findMap. Checking that it intersects is also done similarly, but I'm checking three different possible conditions (either the range contains an interval, or the range contains part of it starting from the source, or part of it ending at the stop).

With that last filter, I get all the map intervals intersecting with my range, which I transform into a list of bounds. I remove any bound that is below or after my range's bounds, and I add these bounds to the list of bounds, before sorting everything together.

With that, I pair every bound with the next bound to get a list of subranges spanning my initial range. Every subrange is now falling into one of the easy case. I now simply need to apply the findMapWithRange function again with these new ranges and concat all the subresults together to get one big result!
