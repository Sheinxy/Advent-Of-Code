{-# LANGUAGE NumericUnderscores #-}
module Main where

import System.Environment
import Data.Either (Either (Right))
import Data.List.Split
import Data.Matrix
import Data.Ratio
import Data.Tuple.Extra
import Text.Regex.TDFA

type Input = [(Matrix Rational, Matrix Rational)]
type Output = Integer

parseInput :: String -> Input
parseInput = map parseMachine . splitOn "\n\n"
    where parseMachine :: String -> (Matrix Rational, Matrix Rational)
          parseMachine machine = (coefs, result)
            where [_ : numStr] = (machine =~ 
                                "Button A: X\\+([0-9]+), Y\\+([0-9]+)\n\
                                \Button B: X\\+([0-9]+), Y\\+([0-9]+)\n\
                                \Prize: X=([0-9]+), Y=([0-9]+)") :: [[String]]
                  numbers = map (fromIntegral . read) numStr
                  coefs   = fromList 2 2 $ take 4 numbers
                  result  = fromList 1 2 $ drop 4 numbers

computeNumPresses :: (Matrix Rational, Matrix Rational) -> Matrix Rational
computeNumPresses (a, y) = multStd y a'
    where Right a' = inverse a

getTokenPrice :: Matrix Rational -> Integer
getTokenPrice a
    | isIntegerSolution = sum $ zipWith (*) (map numerator values) [3, 1]
    | otherwise         = 0
    where values = toList a
          isIntegerSolution = all ((== 1) . denominator) values

partOne :: Input -> Output
partOne = sum . map (getTokenPrice . computeNumPresses)

addOffset :: Matrix Rational -> Matrix Rational
addOffset = elementwise (+) (fromLists [[10_000_000_000_000, 10_000_000_000_000]])

partTwo :: Input -> Output
partTwo = partOne . map (second addOffset)

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args 
