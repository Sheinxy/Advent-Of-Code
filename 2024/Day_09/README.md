## Day 09

My code is awful today

![Disgusted cat](https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Fexternal-preview.redd.it%2FXfHal9-veTmeaAl7rvm-iNXOsBck848UjGwz2k0GrKo.jpg%3Fauto%3Dwebp%26s%3D05a273a50335d9ee9670d7426c7cc4647301d44e&f=1&nofb=1&ipt=00af9fee12dd0ce63fee4d9180c1c23fdf68fa45de4b3efb83f448f0c78637f5&ipo=images)

## The input

The input is pretty simple:
We have digits representing a number of blocks. Even digits represent files, with ID 0, 1, 2 etc.
Odd digits represent empty spaces.

So `1234` means:
```
0..111....
```

I decided to represent the input as a list of (Maybe Int, Int):
- (Just x, i) is a file of ID x and of length i
- (Nothing, i) is an empty space of length i

In order to get that list, I zip each digits with the list [Just 0, Nothing, Just 1, Nothing ...],
which I get by interspesing Nothing with the infinite list of Just i, with I starting from 0.

I filter all entries of length 0 as they serve no purpose:

```hs
parseInput :: String -> Input
parseInput input = filter ((/= 0) . snd) $ zip blockTypes numbers
    where blockTypes = intersperse Nothing [Just i | i <- [0 .. ]]
          numbers    = map digitToInt . head . lines $ input
```

## Part 1

### The problem

I have to try filling empty spaces with the right-most files.

### The solution
I start by getting a queue of files that I can use to fill my spaces with:
```hs
    where endBlocks = reverse (filter (isJust . fst) input)
```

Then I simply go through my whole space:
- If the current element is an empty space I fill it with the current filling chunk
    - If both elements have the same size, it's a perfect match and I can go to the next empty space with the next chunk
    - If the empty space is smaller that the filling chunk, I fill it whole and I reduce the size of my filling chunk
    - If the chunk is smaller than the empty space, I add a new, smaller empty space to my space (correspondings to the blocks that are after the chunk I used to fill the empty space)
- If the current element is not an empty space, I go to the next element
- If the next non-empty chunk in my space is also my filling chunk, then I simply return that chunk and I'm done
- If the next non-empty chunk in my space is beyound my filling chunk, then it means that I have moved all the files that are after my current position in the space, and I must return an empty list.

```hs
moveBlocks :: Input -> Input
moveBlocks input = go input endBlocks
    where endBlocks = reverse (filter (isJust . fst) input)
          go xs ((Just y, i) : _)
            | x == y = [(Just y, i)]
            | x >  y = []
            where Just x = fst . head $ dropWhile (isNothing . fst) xs
          go ((Nothing, i) : xs) ((Just y, j) : ys)
            | i == j    = (Just y, i) : go xs ys
            | i <  j    = (Just y, i) : go xs ((Just y, j - i) : ys)
            | otherwise = (Just y, j) : go ((Nothing, i - j) : xs) ys
          go (h@(Just _, _) : xs) ys = h : go xs ys
```

Once I have that, I can compute the checksum by summing all the IDs by their block index.

I compute the indices by stepping by each entry's length:

```hs
computeChecksum :: Input -> Output
computeChecksum = fst .
                  foldl (\(acc, idx) (x, l) -> 
                          (acc + fromMaybe 0 x * sum [idx .. idx + l - 1],
                           idx + l))
                  (0, 0)

partOne :: Input -> Output
partOne = computeChecksum . moveBlocks
```

## Part 2

### The problem

Now, instead of filling the left-most empty chunks with the right-most files,
I have to move the entire right-most files to the big enough left-most empty chunk.

### The solution

In order to do that, I simply go through my space backwards.

- If I encounter an empty chunk, then I simply keep going
- If I encounter a file chunk, I check if any block before that can fit that file.
 That is any block before is an empty chunk with a length greater than that of the file:
  ```hs
          canFitFileInBlock _ (Just _, _) = False
          canFitFileInBlock (_, i) (Nothing, j) = i <= j 
  ```
  If that is the cace, I split the blocks before in two parts: the part before the
  empty-chunk that will be filled with the file, and the part after. I place the file in that spot, and
  I add a smaller empty-chunk if necessary:
  ```hs
          placeFile file@(Just v, i) xs
            | i < j  = before ++ (file : (Nothing, j - i) : after)
            | i == j = before ++ (file : after)
            where (before, (_, j) : after) = break (canFitFileInBlock file) xs
  ```

```hs
moveEntireBlocks :: Input -> Input
moveEntireBlocks = reverse . go  . reverse
    where go [] = []
          go (x@(Nothing, i) : xs) = x : go xs
          go (x@(Just  _, i) : xs)
            | any (canFitFileInBlock x) xs = (Nothing, i) : go xs'
            | otherwise                    = x : go xs
            where xs' = reverse $ placeFile x $ reverse xs
          canFitFileInBlock _ (Just _, _) = False
          canFitFileInBlock (_, i) (Nothing, j) = i <= j 
          placeFile file@(Just v, i) xs
            | i < j  = before ++ (file : (Nothing, j - i) : after)
            | i == j = before ++ (file : after)
            where (before, (_, j) : after) = break (canFitFileInBlock file) xs

partTwo :: Input -> Output
partTwo = computeChecksum . moveEntireBlocks
```

## The end part

Ew, I hate my code. Usually I mainly use a lot of function composition, but today's solution
felt intrasectly iterative, and I needed to write the recursive function to mimick that iterativeness.
