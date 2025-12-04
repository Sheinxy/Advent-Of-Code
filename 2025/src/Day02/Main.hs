module Day02.Main (day02) where

import           AOC                 (submit)
import           Data.List.Split     (splitOn)
import           Data.Numbers.Primes (primeFactors)
import qualified Data.Set            as S
import           GHC.Utils.Misc      (last2)

type Input = [(Integer, Integer)]
type Output = Integer

parseInput :: String -> Input
parseInput = map (last2 . map read . splitOn "-") . splitOn ","

-- https://oeis.org/A004216
-- a(n) = floor(log_10(n))
-- That's just getting the number of digits in a number
a004216 :: Integer -> Integer
a004216 n = if n <= 9 then 0 else 1 + a004216 (n `div` 10)

-- https://oeis.org/A020338
-- a(n) = n*10^(A004216(n)+1) + n
-- This is just repeating n twice :D
a020338 :: Integer -> Integer
a020338 n = n * 10 ^ (a004216 n + 1) + n

partOne :: Input -> Output
partOne input = sum . filter isInRange . map a020338 $ [1 .. 99999]
    where isInRange n = any (\(a, b) -> a <= n && n <= b) input

-- https://oeis.org/A239019
-- Numbers which are not primitive words over the alphabet {0,...,9} (when written in base 10). 
-- d is the length of the numbers
a239019  :: Integer -> [Integer]
a239019 d = S.toList r'
  where r = S.fromList [x * (10 ^ d - 1) `div` 9 | x <- [1 .. 9]]
        r' = foldl step r . filter (/= d) $ primeFactors d
        step acc p = S.union acc (S.fromList [x * (10 ^ d - 1) `div` (10 ^ q - 1) | x <- [10 ^ (q - 1) .. 10 ^ q - 1]])
            where q = d `div` p

partTwo :: Input -> Output
partTwo input = sum . concatMap (filter isInRange . a239019) $ [2 .. 10]
    where isInRange n = any (\(a, b) -> a <= n && n <= b) input

day02 :: String -> String -> IO ()
day02 "parse" = print . parseInput
day02 "one"   = print . partOne . parseInput
day02 "two"   = print . partTwo . parseInput
day02 "sone"  = submit 2025 2 1 . show . partOne . parseInput
day02 "stwo"  = submit 2025 2 2 . show . partTwo . parseInput
day02 _       = error "Undefined part"
