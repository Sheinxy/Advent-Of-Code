module Main where

import Data.List
import Data.List.Split
import System.Environment

data Mapping = Mapping { start :: Int, end :: Int, diff :: Int } deriving (Show)

type SourceMap = [Mapping]

type Input = ([Int], [SourceMap])
type Output = Int

parseInput :: String -> ([Int], [SourceMap])
parseInput input = (seeds, maps)
    where [seedl] : mapsl = map lines. splitOn "\n\n" $ input
          seeds = map read . words . drop (length "seeds: ") $ seedl
          maps = map (map (getMaps . map read . words) . tail) mapsl
          getMaps [dsr, srs, rl] = Mapping srs (srs + rl) (dsr - srs)

findMap :: Int -> SourceMap -> Int
findMap source map | not . null $ lookup = source + (diff . head $ lookup)
                   | otherwise           = source
                   where lookup = filter (\(Mapping s e _) -> s <= source && source <= e) $ map

partOne :: Input -> Output
partOne (seeds, maps) = minimum . map (\x -> foldl findMap x maps) $ seeds

partTwo :: Input -> Output
partTwo (seeds, maps) = map go . chunksOf 2 $ seeds
-- TODO:
-- POSSIBLE IDEAS:
-- - For a map, split the range into subranges, find the best split and apply the result

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> (readFile $ last args)
    mapM (compute input)  $ init args 
