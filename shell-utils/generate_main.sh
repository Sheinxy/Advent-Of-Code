#!/bin/bash

mkdir -p $1/app
cd $1/app

cat > Main.hs <<EOF
module Main where

import Control.Exception (try, SomeException)
import Control.Monad ((<\$!>))
import Criterion.Measurement (initializeTime, getTime, secs)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.List.Split (splitOn)
EOF

for i in $(seq 1 12); do
    num=`printf %02d $i`
    echo "import Day$num.Main (day$num)" >> Main.hs
done

echo "import Text.Printf (printf)" >> Main.hs
echo "import System.Environment (getArgs)" >> Main.hs
echo "import System.IO (hPrint, hPutStr, hPutStrLn, stderr)" >> Main.hs

echo >> Main.hs
echo "days :: [String -> String -> IO ()]" >> Main.hs
echo -n 'days = [day01' >> Main.hs

for i in $(seq 2 12); do
    num=`printf %02d $i`
    echo -n ", day$num" >> Main.hs
done

echo "]" >> Main.hs
echo >> Main.hs

cat >> Main.hs <<EOF
computePart :: (String -> IO ()) -> String -> IO ()
computePart func part = do
    tStart <- getTime
    result <- try (func part) :: IO (Either SomeException ())
    case result of
        Left e -> hPrint stderr e
        _ -> return ()
    tEnd <- getTime
    let timeTaken = tEnd - tStart
    hPutStr stderr $ "    Part " ++ part ++ ": "
    let colour | timeTaken < 1  = "\ESC[32m"
               | timeTaken < 10 = "\ESC[33m"
               | otherwise      = "\ESC[31m"
    hPutStrLn stderr $ colour ++ secs timeTaken ++ "\ESC[0m"

getDefaultInput :: String -> String
getDefaultInput n = "./$1/inputs/" ++ file
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
          file  | last args \`elem\` ["default", "def", "d"] = getDefaultInput n
                | otherwise = last args
          func  = flip $ days !! (read n - 1)

main :: IO ()
main = do
    initializeTime
    tStart <- getTime
    args  <- splitOn ["/"] <\$!> getArgs
    mapM_ computeDay args
    tEnd <- getTime
    hPutStrLn stderr "Total:"
    let timeTaken = tEnd - tStart
    let colour | timeTaken < 25  = "\ESC[32m"
               | timeTaken < 250 = "\ESC[33m"
               | otherwise       = "\ESC[31m"
    hPutStrLn stderr $ "    " ++ colour ++ secs timeTaken ++ "\ESC[0m"
EOF

stylish-haskell -i Main.hs

cd ..
