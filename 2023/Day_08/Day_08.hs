{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.List
import Data.Map (Map, (!), fromList, keys)
import Text.Regex.TDFA
import System.Environment

data Node = Node { left :: String, right :: String } deriving (Show)

type Input = (String, Map String Node)
type Output = Int

parseInput :: String -> Input
parseInput = (\(inst : _ : nodes) -> (cycle inst, fromList . map getNode $ nodes)) . lines
    where getNode node = (\[idx, l, r] -> (idx, Node l r)) $ tail . head $ (node =~ "(.{3}) = .(.{3}), (.{3})." :: [[String]])

partOne :: Input -> Output
partOne (input, network) = length . takeWhile (/= "ZZZ") . scanl next "AAA" $ input
    where next curr 'R' = right $ network ! curr
          next curr 'L' = left  $ network ! curr

partTwo :: Input -> Output
partTwo (input, network) = foldl1 lcm $ map getLength starting
    where starting  = filter ("A" `isSuffixOf`) $ keys network
          getLength start = length . takeWhile (not . ("Z" `isSuffixOf`)) . scanl next start $ input
          next curr 'R' = right $ network ! curr
          next curr 'L' = left  $ network ! curr

compute :: Input -> String -> IO ()
compute input "parse" = print $ (take 290 $ fst input, snd input)
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> (readFile $ last args)
    mapM (compute input)  $ init args 
