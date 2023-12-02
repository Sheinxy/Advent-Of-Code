{-# LANGUAGE FlexibleContexts #-}
module Main where

-- Compile with -package regex-tdfa
import Text.Regex.TDFA
import System.Environment

data Cube = Red Int | Blue Int | Green Int deriving (Show, Read)

type Input = [[Cube]]
type Output = Int

parseInput :: String -> Input
parseInput = map (map makeCube . matchCube) . lines
    where matchCube s = s =~ "([0-9]+) (red|green|blue)" :: [[String]]
          makeCube [_, n, "red"  ] = Red   (read n)
          makeCube [_, n, "blue" ] = Blue  (read n)
          makeCube [_, n, "green"] = Green (read n)

partOne :: Input -> Output
partOne = sum . map fst . filter (all validate . snd) . zip [1 .. ]
    where validate (Blue  n) = n <= 14
          validate (Green n) = n <= 13
          validate (Red   n) = n <= 12

partTwo :: Input -> Output
partTwo = sum . map (\x -> minimumBlue x * minimumGreen x * minimumRed x)
    where isBlue  (Blue  _) = True
          isBlue  _         = False
          isRed   (Red   _) = True
          isRed   _         = False
          isGreen (Green _) = True
          isGreen _         = False
          minimumBlue  = maximum . map (\(Blue  n) -> n) . filter isBlue
          minimumGreen = maximum . map (\(Green n) -> n) . filter isGreen
          minimumRed   = maximum . map (\(Red   n) -> n) . filter isRed

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> (readFile $ last args)
    mapM (compute input)  $ init args 
