module Main where

import           Data.List
import           Data.List.Split
import           Data.Tuple.Extra
import           System.Environment

type Input = ([[Int]], [[Int]])
type Output = Int

parseInput :: String -> Input
parseInput input = both (map asColCount) schematics
    where schematics = partition (all (== '#') . head) . map lines . splitOn "\n\n" $ input
          asColCount = map (length . head . group) . transpose


partOne :: Input -> Output
partOne (keys, locks) = length [(key, lock) | key <- keys,
                                              lock <- locks,
                                              fits key lock]
    where fits key = and . zipWith (<=) key

partTwo :: Input -> Output
partTwo = error "Not Implemented"

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args
