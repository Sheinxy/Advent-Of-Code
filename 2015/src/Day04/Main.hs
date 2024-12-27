module Day04.Main (day04) where

import           AOC                  (submit)

import           Crypto.Hash
import           Data.ByteString.Lazy (pack)
import           Data.Char

type Input = String
type Output = Int

parseInput :: String -> Input
parseInput = head . lines

solve :: Int -> Input -> Output
solve n input = length . takeWhile (any (/= '0') . take n) . map (show . getMd5) $ [0 .. ]
    where makeInput = (pack . map (fromIntegral . ord)) . (input ++) . show
          getMd5    = hashlazy . makeInput :: Int -> Digest MD5

partOne :: Input -> Output
partOne = solve 5

partTwo :: Input -> Output
partTwo = solve 6


day04 :: String -> String -> IO ()
day04 "parse" = print . parseInput
day04 "one"   = print . partOne . parseInput
day04 "two"   = print . partTwo . parseInput
day04 "sone"  = submit 2015 4 1 . show . partOne . parseInput
day04 "stwo"  = submit 2015 4 2 . show . partTwo . parseInput
day04 _       = error "Undefined part"
