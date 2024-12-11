module Main where

import System.Environment
import Data.List
import Data.Tuple.Extra
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Input  = Map Int Int
type Output = Int

parseInput :: String -> Input
parseInput input = Map.fromList compressedNumbers
    where numbers           = map read . words $ input
          compressedNumbers = map (\l -> (head l, length l)) . group . sort  $ numbers
          
blink :: Input -> Input
blink = Map.foldrWithKey transformStone Map.empty
    where addStone count Nothing       = Just count
          addStone count (Just count') = Just (count + count')
          transformStone stone count res
            | stone == 0               = Map.alter (addStone count) 1 res
            | (even . length) stoneStr = Map.alter (addStone count) rightHalf $ Map.alter (addStone count) leftHalf res
            | otherwise                = Map.alter (addStone count) (stone * 2024) res
            where stoneStr = show stone
                  (leftHalf, rightHalf) = (read *** read) $ splitAt (length stoneStr `div` 2) stoneStr

partOne :: Input -> Output
partOne = sum . Map.elems . (!! 25) . iterate blink 

partTwo :: Input -> Output
partTwo = sum . Map.elems . (!! 75) . iterate blink 

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args 
