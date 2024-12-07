## Day 07

I keep bruteforcing and it keeps working!

I will be quick here because it is quite simple and I don't have much time

## The input

I split each line on ": ", giving me two parts:
- One is the target number
- One is a space-separated list of numbers.

I read the target number, and I split the list and read every number:

```hs
parseInput :: String -> Input
parseInput = map ((\[a, b] -> (read a, map read $ words b)) . splitOn ": ") . lines
```

## Part one and two

I simply try every possible operation until I no longer have any number in the list.
When the list is empty, I return True if the target was found, False otherwise.
A target is deducible if bruteforcing all operations lead to one True result.

I filter all the deducible lines, and sum their targets:

```hs
partOne :: Input -> Output
partOne = sum . map fst . filter isDeducible
    where isDeducible (target, x : xs) = testOperations target x xs
          testOperations target acc [] = target == acc
          testOperations target acc (x : xs) = testOperations target (acc + x) xs ||
                                               testOperations target (acc * x) xs

partTwo :: Input -> Output
partTwo = sum . map fst . filter isDeducible
    where isDeducible (target, x : xs) = testOperations target x xs
          testOperations target acc [] = target == acc
          testOperations target acc (x : xs) = testOperations target (acc + x) xs ||
                                               testOperations target (acc * x) xs ||
                                               testOperations target (read $ show acc ++ show x) xs
```

The only weird operator is "||", which I simply create by converting all numbers to strings, concatenating them and converting the result back to an int. (Not optimised but it works and was fast to write)

## The end

Well that was easy. And the runtime is not that long tbh, like 2 seconds.
