## Day 07

I suck at poker.

Today was the "I want to do Instances to solve this" day!

```hs
import Data.List (group, sort)
import Data.Map (Map, (!), fromList)
import Data.Ord (compare)
import Data.Text (replace, unpack, pack)
import Data.Tuple.Extra (both)
import System.Environment

data Hand = Hand { cards :: String, bid :: Int, hType :: Type, order :: String} deriving (Eq, Show)
data Type = High | One | Two | Three | Full | Four | Five deriving (Eq, Ord, Show)

instance Ord Hand where
    (Hand c1 _ t1 o1) `compare` (Hand c2 _ t2 o2) 
        | o1 /= o2  = error "Comparing different games"
        | t1 /= t2  = t1 `compare` t2
        | otherwise = uncurry compare $ both (map (cardStrength !)) (c1, c2)
        where cardStrength = fromList $ zip o1 [1 .. ]

type Input = [Hand]
type Output = Int

getType :: String -> Type
getType = go . sort . map length . group . sort
    where go [5]          = Five
          go [1, 4]       = Four
          go [2, 3]       = Full
          go [1, 1, 3]    = Three
          go [1, 2, 2]    = Two
          go [1, 1, 1, 2] = One
          go _            = High

getJokeType :: String -> Type
getJokeType s = maximum . map getType . map replaceJ $ "23456789TQKA"
    where replaceJ c = unpack . replace "J" (pack [c]) . pack $ s

parseInput :: String -> Input
parseInput =  map ((\[c, b] -> Hand c (read b) (getType c) "23456789TJQKA") . words) . lines

partOne :: Input -> Output
partOne = sum . map (uncurry (*)) . zip [1 .. ] . map bid . sort

partTwo :: Input -> Output
partTwo = partOne . map convertHand
    where convertHand h = Hand (cards h) (bid h) (getJokeType $ cards h) "J23456789TQKA"
```

Alright, so first and foremost I would like to apologise: I have failed. My solution is way longer than needed:

I could have simply make my Hand data structure be the following:
```hs
data Hand = Hand { hType :: Type, cards :: [Int], bid :: Int } deriving (Eq, Ord, Show)
```

With cards being the list of values of each card in the hand. By doing that I would not have needed to specify how to compare two hands.

But anyway, what is done is done, so let's look back at my actual data structure:
```hs
data Hand = Hand { cards :: String, bid :: Int, hType :: Type, order :: String} deriving (Eq, Show)
data Type = High | One | Two | Three | Full | Four | Five deriving (Eq, Ord, Show)

instance Ord Hand where
    (Hand c1 _ t1 o1) `compare` (Hand c2 _ t2 o2) 
        | o1 /= o2  = error "Comparing different games"
        | t1 /= t2  = t1 `compare` t2
        | otherwise = uncurry compare $ both (map (cardStrength !)) (c1, c2)
        where cardStrength = fromList $ zip o1 [1 .. ]
```
A Hand is a string representing the cards, an Int being the bid on that hand, the type of hand, and a string representing the order of each card (basically, the weakest cards are first in the string).

A type is pretty self explanatory if you read the subject. They are defined from weakest (High) to strongest (Five)

To compare two hands:
 - If they have different card strengths, then they cannot be compared
 - If their types differ then simply compare the type
 - Otherwise, compare their cards by mapping each card to its strength (index in the order string) and comparing the two lists: this basically compares the first elements that differ in the lists.


To get my list of hands (my Input), I use the following:
```hs
getType :: String -> Type
getType = go . sort . map length . group . sort
    where go [5]          = Five
          go [1, 4]       = Four
          go [2, 3]       = Full
          go [1, 1, 3]    = Three
          go [1, 2, 2]    = Two
          go [1, 1, 1, 2] = One
          go _            = High

parseInput :: String -> Input
parseInput =  map ((\[c, b] -> Hand c (read b) (getType c) "23456789TJQKA") . words) . lines
```
Each line of my puzzle input represents a list of card and a bid, separated by a space. So, for each line, I split by spaces, which gives me two elements (my cards and my bid).

I put these two elements in my data structure (converting the bid into an Int), with the type of hand that I get using getType on the cards, and the default order for the cards.

The getType function starts by sorting the cards and grouping them together (so if my input is "23K32" I get ["22", "33", "K"]).

Then I get the number of cards there are in each group, and I sort by increasing number of cards (so I now have [1, 2, 2]).

Then I simply map each possible result to its type (this is just reading the puzzle and literally writing down what it tells: here I have two pairs and one lone card so my hand is a Two Pairs).


Now to solve part 1, everything is pretty easy:
```hs
partOne :: Input -> Output
partOne = sum . map (uncurry (*)) . zip [1 .. ] . map bid . sort
```
I simply sort my hands from weakest to strongest, and I get the bid of each hand. I assign each bid its rank (the rank of the hand, as the bids are sorted the same way the hands were),
 and I multiply each bid with its rank. I sum all the results and I get my answer.


Part two is pretty easy as well:
```hs
partTwo :: Input -> Output
partTwo = partOne . map convertHand
    where convertHand h = Hand (cards h) (bid h) (getJokeType $ cards h) "J23456789TQKA"
```
I simply start by converting my hands to make them fit the new rules: the cards and bids don't change, but the way to find the type and the cards' order change.

To get the new type I do the following:
```hs
getJokeType :: String -> Type
getJokeType s = maximum . map getType . map replaceJ $ "23456789TQKA"
    where replaceJ c = unpack . replace "J" (pack [c]) . pack $ s
```
This is actually not really smart (it's bruteforce). I could have looked for the most common card that is not a joker and use it as my joker replacement,
but instead I simply replace every joker with every single card type and I get the resulting type, and I keep the one that gives the best type.

It is interesting to note that I'm always replacing every joker with the same card type: I have not proven that it will always give the best result, but I highly strongly really pretty much 99% confidently believe that it does. My reasoning being that the type of hands with more cards of the same type are always better than the ones with multiple smaller groups of cards. (e.g, it is better to have [1, 1, 3] than [1, 2, 2])

And that's about it. Now that everything is converted, I simply call part one again, as the algorithm doesn't change thanks to the fact that the way to order hands is the same.

## BONUS ROUND: I DID IT WITHOUT WRITING INSTANCE ONCE:
```hs
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List
import Data.Map (fromList, (!))
import Data.Text (replace, unpack, pack)
import System.Environment

data Hand = Hand { hType :: Type, cards :: [Int], bid :: Int, raw :: String } deriving (Eq, Ord, Show)
data Type = High | One | Two | Three | Full | Four | Five deriving (Eq, Ord, Show)

type Input = [Hand]
type Output = Int

getType :: String -> Type
getType = go . sort . map length . group . sort
    where go [5]          = Five
          go [1, 4]       = Four
          go [2, 3]       = Full
          go [1, 1, 3]    = Three
          go [1, 2, 2]    = Two
          go [1, 1, 1, 2] = One
          go _            = High

getCards :: String -> String -> [Int]
getCards strength = map (mapping !)
    where mapping = fromList $ zip strength [1 .. ]

parseInput :: String -> Input
parseInput =  map ((\[c, b] -> Hand (getType c) (getCards "23456789TJQKA" c) (read b) c) . words) . lines

partOne :: Input -> Output
partOne = sum . map (uncurry (*)) . zip [1 .. ] . map bid . sort

getJokeType :: String -> Type
getJokeType s = maximum . map getType . map replaceJ $ "23456789TQKA"
    where replaceJ c = unpack . replace "J" (pack [c]) . pack $ s

partTwo :: Input -> Output
partTwo = partOne . map convertHand
    where convertHand h = h { hType = (getJokeType $ raw h), cards = (getCards "J23456789TQKA" $ raw h) }
```
