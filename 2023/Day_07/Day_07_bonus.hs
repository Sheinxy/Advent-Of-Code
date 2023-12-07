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

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> (readFile $ last args)
    mapM (compute input)  $ init args 
