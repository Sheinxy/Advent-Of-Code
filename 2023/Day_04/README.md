## Day 04

Simple day, I am a happy cat 😸

```hs
import Data.List (break, replicate)
import Data.Map (Map, adjust, toList, (!))
import qualified Data.Map as M (fromList)
import Data.Set (Set, fromList, intersection, size)
import System.Environment

data Card = Card { winning :: Set Int, numbers :: Set Int } deriving (Show)

type Input = [Card]
type Output = Int

parseInput :: String -> Input
parseInput = map (getNumbers . break (== '|') . drop (length "Card   1:")) . lines
    where getNumbers (w, _ : n) = Card (fromList . map read $ words w) (fromList . map read $ words n)

partOne :: Input -> Output
partOne = sum . map ((2 ^) . (+ (-1))) . filter (/= 0) . map (size . (\(Card w n) -> w `intersection` n))

partTwo :: Input -> Output
partTwo card = sum . map snd . toList . foldl go startingState $ zip [1 .. ] card
    where startingState = M.fromList . zip [1 .. ] $ replicate (length card) 1
          go m (id, Card w n) = foldl (flip $ adjust (+ (m ! id))) m [id + 1 .. id + size (w `intersection` n)]
```

Let's look a little bit at what I did:
```hs
data Card = Card { winning :: Set Int, numbers :: Set Int } deriving (Show)

type Input = [Card]
type Output = Int

parseInput :: String -> Input
parseInput = map (getNumbers . break (== '|') . drop (length "Card   1:")) . lines
    where getNumbers (w, _ : n) = Card (fromList . map read $ words w) (fromList . map read $ words n)
```

I have a data structure to represent a card. A card is a set of winning numbers and a set of picked numbers (I assumed that numbers are unique in both side of the card, which seems to be the case).

To get my list of cards (ie my input), I start, as always, by getting all the lines from the file. For each line, I first get rid of the "Card XXX:" part, as I am not interested in that part (and I can always find it back by using the index of the card in the list).

Using the break function, I can split my strings in two parts: the winning numbers and the picked numbers. For example, if my current line is (after removing the Card XXX:) "42 12 | 7 81", I would get back the pair ("42 12 ", "| 7 81").

From that pair, I can get the numbers by simply splitting both strings by spaces and converting the strings into integers. I still need to get read of the leading "|" in the first pair though. I convert both list into sets for convenience (and faster computation)

Now, let's compute the first answer:
```hs
partOne :: Input -> Output
partOne = sum . map ((2 ^) . (+ (-1))) . filter (/= 0) . map (size . (\(Card w n) -> w `intersection` n))
```

The puzzle says that the score of a card increases with each matching number: it starts at 1 for the first matching number and doubles for each subsequent one. If you like arithmetic, you should be quick to notice that this is basically 2^(winning numbers - 1)! 😸

Henceforth (fancy word), I start by getting the size of the intersection between the winning set and the picked numbers set for each card. I remove those with 0 winning numbers (they are not interesting), and for the other I compute the score by using the formula 2^(winning - 1). I sum all the scores together and voilà!

The second puzzle is a bit more tricky, as it feels like it would be great to solve it using "Dynamic programming" (aka an array), luckily enough a Map is fine too 😸:
```hs
partTwo :: Input -> Output
partTwo card = sum . map snd . toList . foldl go startingState $ zip [1 .. ] card
    where startingState = M.fromList . zip [1 .. ] $ replicate (length card) 1
          go m (id, Card w n) = foldl (flip $ adjust (+ (m ! id))) m [id + 1 .. id + size (w `intersection` n)]
```

My starting state is a Map, mapping cards ids to the number of cards I have with said id. I start with 1 card for each id.

Remember earlier when I said I wasn't keeping track of my cards' ids, well this is what the zip [1 .. ] is now for, like that I can map each card with their id.

Now, for each card (mapped with their id), I update my map (starting state) by applying the go function:

For every card number between id + 1 and id + number of winning numbers, I add the number of cards of the current id that I have to my Map. E.g if my current id is 4, that I have 2 cards of id 4, and that I have 3 winning numbers, then I'm going to do +2 on cards 5, 6 and 7.

I'm also abusing Haskell by taking advantage of the facts that:
 - If I don't have any winning numbers, then [id + 1 .. id] is going to give me []
 - adjust just returns the Map if the key doesn't exists, so I don't need to bound id + winning numbers

 After I have computed the number of cards, I simply sum each number to get my answer!

 :cool_sheinxy emoji here: 
