module Main where

import           Control.Exception     (SomeException, try)
import           Control.Monad         ((<$!>))
import           Criterion.Measurement (getTime, initializeTime, secs)
import           Data.Char             (isSpace)
import           Data.List             (dropWhileEnd)
import           Data.List.Split       (splitOn)
import           Day01.Main            (day01)
import           Day02.Main            (day02)
import           Day03.Main            (day03)
import           Day04.Main            (day04)
import           Day05.Main            (day05)
import           Day06.Main            (day06)
import           Day07.Main            (day07)
import           Day08.Main            (day08)
import           Day09.Main            (day09)
import           Day10.Main            (day10)
import           Day11.Main            (day11)
import           Day12.Main            (day12)
import           System.Environment    (getArgs)
import           System.IO             (hPrint, hPutStr, hPutStrLn, stderr)
import           Text.Printf           (printf)

days :: [String -> String -> IO ()]
days = [day01, day02, day03, day04, day05, day06, day07, day08, day09, day10, day11, day12]

computePart :: (String -> IO ()) -> String -> IO ()
computePart func part = do
    tStart <- getTime
    result <- try (func part) :: IO (Either SomeException ())
    case result of
        Left e -> hPrint stderr e
        _      -> return ()
    tEnd <- getTime
    let timeTaken = tEnd - tStart
    hPutStr stderr $ "    Part " ++ part ++ ": "
    let colour | timeTaken < 1  = "\ESC[32m"
               | timeTaken < 10 = "\ESC[33m"
               | otherwise      = "\ESC[31m"
    hPutStrLn stderr $ colour ++ secs timeTaken ++ "\ESC[0m"

getDefaultInput :: String -> String
getDefaultInput n = "./2025/inputs/" ++ file
    where n'   = read n :: Int
          file = printf "%02d" n' :: String

defaultArgs :: [[String]]
defaultArgs = map (\ n -> [n, "one", "two", getDefaultInput n]) dayStrings
    where dayStrings :: [String]
          dayStrings = map (printf "%02d") ([1 .. 12] :: [Int])

computeDay :: [String] -> IO ()
computeDay [] = mapM_ computeDay defaultArgs
computeDay [n] = computeDay [n, "one", "two", getDefaultInput n]
computeDay [n, file] = computeDay [n, "one", "two", file]
computeDay (n : args) = do
       hPutStrLn stderr $ "Day " ++ n ++ ":"
       content <- try (readFile file) :: IO (Either SomeException String)
       case content of
        Left e -> hPrint stderr e
        Right content' -> do
           let input = dropWhileEnd isSpace content'
           mapM_ (computePart $ func input) parts
    where parts = init args
          file  | last args `elem` ["default", "def", "d"] = getDefaultInput n
                | otherwise = last args
          func  = flip $ days !! (read n - 1)

main :: IO ()
main = do
    initializeTime
    tStart <- getTime
    args  <- splitOn ["/"] <$!> getArgs
    mapM_ computeDay args
    tEnd <- getTime
    hPutStrLn stderr "Total:"
    let timeTaken = tEnd - tStart
    let colour | timeTaken < 25  = "\ESC[32m"
               | timeTaken < 250 = "\ESC[33m"
               | otherwise       = "\ESC[31m"
    hPutStrLn stderr $ "    " ++ colour ++ secs timeTaken ++ "\ESC[0m"
