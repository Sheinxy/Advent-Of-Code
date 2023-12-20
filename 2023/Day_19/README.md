## Day 19

Today's puzzle was brought to you by the "where" keyword :d

It is once again quite late as I am writing that, so I'll be quick!

## The input (aka the ugly):

Parsing the input here was quite a challenge. I started by declaring three data structures:

```hs
data Ratings   = Ratings   { x :: Int, m :: Int, a :: Int, s :: Int    } deriving (Read)
data Rule      = Rule      { condition :: Condition, ifTrue  :: String }
data Condition = Condition { operation :: Int -> Bool, field :: Char   }

type Input  = (Map String [Rule], [Ratings])
```

Ratings correspond to the parts in the problem (don't ask me. I suck at naming stuff). They're the second half of the input file.

In my parseInput function I have a subfunction that takes a part line from the input file and parses it as my Ratings structure by using some Haskell magic:
```hs
parseRatings   = read . ("Ratings " ++)
```

This works because Ratings is deriving Read, which means that it implements the default read function (aka a from string function),
which basically takes a string representing how you would create such a structure in code and transforms it into the actual structure:

```hs
read "Ratings { x=1, m=2, a=3, s=4 }" -- This gives a Ratings structure.
```

Workflows are a named list of rules. I represent them using a Map with the name as the key and the rules as the value.

To parse the workflows, I replace every '{', '}' and ',' with spaces. I then split by whitespaces, which gives me a name as the first element, and each rule as the subsequent elements.

In order to parse a rule, I split on ':'. If the right part is empty (ie. there was no ':') then the condition is to always return true. Otherwise I need to parse the condition by getting the operation to apply:
```hs
parseWorkflow  = (\(name:rules) -> (name, map parseRule rules)) . words .
                                                                 replace "}" " " .
                                                                 replace "{" " " .
                                                                 replace "," " "
parseRule rule | not . null $ ifTrue = Rule (parseCond cond) $ tail ifTrue    -- If there is a condition: get the op
               | otherwise           = Rule (Condition (const True) 'x') cond -- If there is no condition (last rule): always return true
                where (cond, ifTrue) = break (`elem` ":") rule
parseCond cond = Condition (getOp op num) rating
              where ([rating], op:num) = break (`elem` "<>") cond
                    getOp '<' num = (< read num)
                    getOp '>' num = (> read num)
```

Now, to parse the whole input, I split my file by lines, and I separate those lines into two parts: one before the empty line and one after.

Then, the first part represents my workflows, so I apply the parseWorkflow function on each line.
The second part are my Ratings, plus the empty line. I remove the empty line and I apply my parseRatings on each rating:

```hs
parseInput :: String -> Input
parseInput = ((fromList . map parseWorkflow) *** (map parseRatings . tail)) . break null . lines
```

## Part one (the good):

First of all, in order to get the value of a field of my Ratings structure based on a Char value, I define a getField function:

```hs
getField :: Char -> (Ratings -> Int)
getField 'a'  = a
getField 'm'  = m
getField 's'  = s
getField 'x'  = x
```

This is because my Condition structure works with the operee (is that a word? the thing we apply the operation onto) represented by a char.

Next, I define a getNextState function. This function takes a Ratings structure (a part), and a list of rules representing the current workflow. It yields the name of the next workflow that we get by applying the rules onto the part.

In order to do that, I simply check each rule against my part until I find one for which the condition is true. The returned result is the target for that rule
```hs
getNextState :: Ratings -> [Rule] -> String
getNextState ratings = ifTrue . head . dropWhile (not . checkRule)
    where checkRule (Rule (Condition op rating) _) = (op . getField rating) ratings
```

Now to get the acceptation (Accepted or Rejected) of a part, I simply iterate getting the next state for the rating by starting at the "in" workflow. I do that until my current workflow is either "A" or "R", and I return that last workflow:
```hs
getAcceptation :: Map String [Rule] -> Ratings -> String
getAcceptation workflows ratings = head . dropWhile (`notElem` ["A", "R"]) .
                                   iterate (getNextState ratings . (workflows !)) $ "in"
```

Now to get my answer, I simply get the acceptation of each part, and I keep the accepted ones. I sum the values of the xmas field of each accepted par, and I sum all of the results together:
```hs
partOne :: Input -> Output
partOne (workflows, ratings) = sum . map getRatingsValue . filter (("A" ==) . getAcceptation workflows) $ ratings
    where getRatingsValue ratings = sum . map ($ ratings) $ [a, m, s, x]
```

## Part two (the bad):

Small challenge for you. try to understand this:
```hs
-- Returns a list of Map. Each element of the list represent combinations leading to an accepting state
followPathsToAcceptance :: Map String [Rule] -> [Map Char [Int]]
followPathsToAcceptance workflows = go "in" . fromList . map (, [1 .. 4000]) $ "amsx"
    where go "A" ranges = [ranges]                           -- This is an accepting state: yield the accepting numbers
          go "R" _      = [      ]                           -- This is a rejecting state: yield no number
          go _  ranges  | all null . elems $ ranges = []     -- This is an impossible state: yield no number
          go cur ranges = tryPaths (workflows ! cur) ranges  -- For a regular state, follow every possible path
            where tryPaths [] _            = []              -- If there is no path left to follow, stop the loop
                  -- Try to follow the current rule. Try to reject it. Combine the two results:
                  tryPaths (r : rs) ranges = go (ifTrue r) trueRanges ++ tryPaths rs falseRanges
                      where cond        = condition r
                            op          = operation cond
                            rating      = field cond
                            trueRanges  = adjust (filter        op)  rating ranges -- The numbers that allow to follow the rule
                            falseRanges = adjust (filter (not . op)) rating ranges -- The numbers that allow to reject the rule
```

What this function does is that it starts at the workflow "in" with a mapping with the "xmas" field as the key and the list of valid numbers for that field as the value. Basically, we start by assuming that every number between 1 and 4000 for every field of a part is valid to get to the "in" workflow (which is true, as there is no condition to get there).

Next what we're going to do is that we're going to follow every possible path, filtering out numbers that would make it impossible to get to that path.

When we arrived on the "A" workflow, then our numbers represent valid values for the xmas field that can lead to this workflow with the specific path taken (each path has unique combinations leading to them 😸).

Similarly, arriving on the "R" workflow, our arriving at a point where we no longer have valid numbers in our fields simply yield no number: following this path is not a valid way to accept the part.

For any other regular workflow, we want to check every rule:
    - Either we follow the path of that rule, therefore we need to keep the numbers satisfying the condition described by that rule and go on to the next workflow
    - Or we reject that rule and we go on the next rules, therefore we need to keep the numbers that do not satisfy the condition described by that rule and go on to the next rule (for which we will also apply both choices etc. etc.)

We combine the result for each path we take to get the combinations leading to an accepting state by following that path.

Now that we have the combinations, we need to compute their actual number:

Imagine you have three slots that can take respectively 3, 5 and 2 values: ABC. A can have 3 values. For each value of A, B can have 5 values. For each value of B, C can have 2 values. So in total, there are 3 * 5 * 2 = 30 possible combinations.

This is the same idea here: for each field, check the number of valid numbers, multiply all of them together to get the possibilities for one path, and add the number of possibilites (as each path is unique there will be no overlap):
```hs
partTwo :: Input -> Output
partTwo = sum .
          map (product . map length . elems) .
          followPathsToAcceptance .
          fst
```

And that's all! 🐈‍⬛
