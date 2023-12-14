module Main where

import Crypto.Hash
import Data.ByteString.Lazy (pack)
import Data.Char
import System.Environment

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

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args 
