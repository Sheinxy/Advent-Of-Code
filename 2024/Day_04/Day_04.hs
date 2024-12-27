module Main where

import           Data.List
import           System.Environment
import           Text.Regex.TDFA

type Input = [String]
type Output = Int

parseInput :: String -> Input
parseInput = lines

-- Not the same as Data.List.Split's chunksOf
-- This gives all the complete subsequences of length n
-- That is like iterating over take (length l - x) $ drop x l
chunksOf :: Int -> [a] -> [[a]]
chunksOf n = concatMap (filter ((== n) . length) . inits) . tails

partOne :: Input -> Output
partOne input = searchHorizontal input + searchVertical input + searchDiagonal input
    where search :: String -> [String]
          search l = concat (l =~ "XMAS" :: [[String]])
          searchHorizontal l = length $ concatMap (search . reverse) l ++ concatMap search l
          searchVertical     = searchHorizontal . transpose
          searchDiagonal  l  = countInDiagonal l + countInDiagonal (reverse l)
            where shift l         = [drop i x | (i, x) <- zip [0 .. ] l]
                  getDiagonals    = map shift . chunksOf 4
                  countInDiagonal = sum . map searchVertical . getDiagonals


partTwo :: Input -> Output
partTwo =  length . filter id . concatMap (zipWith3' isCross . map (chunksOf 3)) . chunksOf 3
    where zipWith3' f [a, b, c] = zipWith3 f a b c
          isCross ['M', _, 'S'] [_, 'A', _] ['M', _, 'S'] = True
          isCross ['S', _, 'M'] [_, 'A', _] ['S', _, 'M'] = True
          isCross ['M', _, 'M'] [_, 'A', _] ['S', _, 'S'] = True
          isCross ['S', _, 'S'] [_, 'A', _] ['M', _, 'M'] = True
          isCross _ _ _                                   = False

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args
