module Day05.Main (day05) where

import           AOC             (submit)
import           Data.List
import           Data.List.Split (chunksOf, divvy)

type Input = [String]
type Output = Int

parseInput :: String -> Input
parseInput = lines

isNice :: String -> Bool
isNice s = all ($ s) [hasThreeVowels, hasTwiceInRow, doesntContain]
    where hasThreeVowels :: String -> Bool
          hasThreeVowels = (>= 3) . length . filter (`elem` "aeiou")

          hasTwiceInRow :: String -> Bool
          hasTwiceInRow = any ((>= 2) . length) . group

          doesntContain :: String -> Bool
          doesntContain s = not . any (`isInfixOf` s) $ ["ab", "cd", "pq", "xy"]

partOne :: Input -> Output
partOne = length . filter isNice

isNice' :: String -> Bool
isNice' s = all ($ s) [hasPairs, hasRepeat]
    where hasPairs :: String -> Bool
          hasPairs = any (\ s -> take 2 s `isInfixOf` drop 2 s) . takeWhile (not . null) . iterate tail

          hasRepeat :: String -> Bool
          hasRepeat = any (\ [a, _, b] -> a == b) . divvy 3 1


partTwo :: Input -> Output
partTwo = length . filter isNice'

day05 :: String -> String -> IO ()
day05 "parse" = print . parseInput
day05 "one"   = print . partOne . parseInput
day05 "two"   = print . partTwo . parseInput
day05 "sone"  = submit 2015 5 1 . show . partOne . parseInput
day05 "stwo"  = submit 2015 5 2 . show . partTwo . parseInput
day05 _       = error "Undefined part"
