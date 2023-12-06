module Main where

import Data.Tuple.Extra
import System.Environment

type Input = [(Integer, Integer)]
type Output = Int

parseInput :: String -> Input
parseInput = uncurry zip . go . lines
    where go [t, d] = both (map read . tail . words) (t, d)

getBounds :: (Integer, Integer) -> (Int, Int)
getBounds (t, d) = (ceiling (t2 - 1), floor (t1 + 1))
    where delta      = t * t - 4 * d
          root       = sqrt(fromIntegral delta)
          (t1 , t2)  = ((fromIntegral t - root) / 2, (fromIntegral t + root) / 2)

partOne :: Input -> Output
partOne = product . map ((+ 1) . uncurry (-)) . map getBounds

partTwo :: Input -> Output
partTwo = partOne . (: []) . both (read . concatMap show) . unzip

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> (readFile $ last args)
    mapM (compute input)  $ init args 
