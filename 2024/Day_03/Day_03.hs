module Main where

import System.Environment
import Text.Regex.TDFA

type Input = String
type Output = Int

partOne :: Input -> Output 
partOne =  sum . map mul . (=~ "mul\\(([0-9]+),([0-9]+)\\)")
    where mul [_, x, y] = read x * read y

partTwo :: Input -> Output
partTwo = snd .
         foldl run (True, 0) .
         ((=~ "do\\(\\)|don't\\(\\)|mul\\(([0-9]+),([0-9]+)\\)") :: String -> [[String]])
    where run (_, acc) ["do()", _, _] = (True, acc)
          run (_, acc) ["don't()", _, _] = (False, acc)
          run (False, acc) _ = (False, acc)
          run (True, acc) [_, x, y] = (True, acc + read x * read y)

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- readFile (last args)
    mapM (compute input) $  init args 
