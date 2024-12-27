module Main where

import           Control.Parallel.Strategies
import           Data.Bits
import           Data.List
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           System.Environment

type Input = [Int]
type Output = Int

parseInput :: String -> Input
parseInput = map read . lines

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = takeWhile ((n ==) . length) . map (take n) . tails

computeNextSecret :: Int -> Int
computeNextSecret x = res
    where y   = ((x `shift`   6)  `xor` x) .&. 0x00FFFFFF
          z   = ((y `shift` (-5)) `xor` y) .&. 0x00FFFFFF
          res = ((z `shift`  11)  `xor` z) .&. 0x00FFFFFF

getSecrets :: [Int] -> [[Int]]
getSecrets = map (take 2001 . iterate computeNextSecret)

partOne :: Input -> Output
partOne = sum . map last . getSecrets

computeSequences :: [Int] -> Map [Int] Int
computeSequences secrets = Map.fromListWith (\_ x -> x) . map arrange $ chunks
    where zipDiff a b = (a, b - a)
          arrange l   = (map snd l, (fst . last) l)
          digits  = map (`mod` 10) secrets
          diffs   = zipWith zipDiff (tail digits) digits
          chunks  = chunksOf 4 diffs

partTwo :: Input -> Output
partTwo input = (Set.findMax . Set.map bananas) sequences
    where bananas sequence = sum . parMap rseq (Map.findWithDefault 0 sequence) $ mappings
          secrets   = getSecrets input
          mappings  = parMap rseq computeSequences secrets
          sequences = Set.fromList (mappings >>= Map.keys)

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args
