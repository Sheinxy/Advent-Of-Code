{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Function (on)
import Data.List (group, sort)
import Data.Map (Map, (!), fromList)
import Data.Ord (compare)
import Data.Text (replace, unpack, pack)
import System.Environment

data Hand = Hand { cards :: String, bid :: Int, hType :: Type, order :: String} deriving (Eq, Show)
data Type = High | One | Two | Three | Full | Four | Five deriving (Eq, Ord, Show)

instance Ord Hand where
    (Hand c1 _ t1 o1) `compare` (Hand c2 _ t2 o2) 
        | o1 /= o2  = error "Comparing different games"
        | t1 /= t2  = t1 `compare` t2
        | otherwise = uncurry (compare `on` (cardStrength !)) .
                      head . dropWhile (uncurry (==)) $ zip c1 c2
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

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> (readFile $ last args)
    mapM (compute input)  $ init args 
