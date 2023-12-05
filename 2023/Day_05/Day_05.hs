module Main where

import Data.List
import Data.List.Split
import System.Environment

data Map = Map { start :: Int, end :: Int, diff :: Int } deriving (Show)

type SourceMap = [Map]

type Input = ([Int], [SourceMap])
type Output = Int

parseInput :: String -> Input
parseInput input = (seeds, maps)
    where [seedl] : mapsl        = map lines . splitOn "\n\n" $ input
          seeds                  = map read  . words . drop (length "seeds: ") $ seedl
          maps                   = map (map (getMaps . map read . words) . tail) mapsl
          getMaps [dsr, srs, rl] = Map srs (srs + rl) (dsr - srs)

findMap :: Int -> SourceMap -> Int
findMap source smap | not . null $ lookup = source + (diff . head $ lookup)
                    | otherwise           = source
                    where lookup = filter (\(Map s e _) -> s <= source && source < e) smap

partOne :: Input -> Output
partOne (seeds, maps) = minimum . map (\x -> foldl findMap x maps) $ seeds

findMapWithRange :: SourceMap -> (Int, Int) -> [(Int, Int)]
findMapWithRange smap (source, stop) 
    | isInInterval   = [(source + inDiff, stop + inDiff)] -- Range is in an interval,new range is linear application
    | spansIntervals = concatMap (findMapWithRange smap) newIntervals -- Range spans some intervals, we apply on the subintervals
    | otherwise      = [(source, stop)] -- Range has no part in common with an interval, so it doesn't change
        where inLookup = filter (\(Map s e _) -> s <= source && stop <= e) smap    -- Case 0: The range is inside an interval
              inDiff   = diff . head $ inLookup
              -- Looking for ranges covered by the seeds
              covRanges      = filter (\(Map s e _) -> (source < s  && e < stop)   ||   -- Case 1: Interval is fully in range
                                                       (s <= source && source < e) ||   -- Case 2: Start of range is in interval
                                                       (s < stop    && stop <= e)) smap -- Case 3: End of range is in interval
              bounds         = sort . ([source, stop] ++) . filter (\n -> source < n && n < stop) . concatMap (\m -> [start m, end m]) $ covRanges
              newIntervals   = zip bounds (tail bounds) -- Subintervals spanning (source, stop(
              -- Aliases to describe the two cases:
              isInInterval   = not . null $ inLookup  -- The range is fully in an interval
              spansIntervals = not . null $ covRanges -- The range is partially in an interval

partTwo :: Input -> Output
partTwo (seeds, maps) = minimum . map fst . concatMap go $ chunksOf 2 seeds
    where go [s, r] = foldl (\x m -> concatMap (findMapWithRange m) x) [(s, s + r)] maps

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> (readFile $ last args)
    mapM (compute input)  $ init args 
