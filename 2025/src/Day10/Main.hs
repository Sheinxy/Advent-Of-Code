module Day10.Main (day10) where

import           AOC              (submit)
import           Control.Monad    (forM, (<=<))
import           Data.Bits
import           Data.Foldable    (for_)
import           Data.List        (elem, find, sortOn)
import           Data.Maybe       (fromJust)
import           Data.SBV
import           Data.Traversable (for)

data Instruction = Instruction { light :: Int, buttons :: [[Int]], joltage :: [Int] } deriving Show

type Input = [Instruction]
type Output = Int

parseLine :: String -> Instruction
parseLine raw = Instruction lights buttons joltages
    where (lightRaw, ' ' : rest) = break (== ' ') raw
          (buttonsRaw, rest') = break (== '{') rest
          buttons = map (\x -> read $ "[" ++ init (tail x) ++ "]") $ words buttonsRaw
          joltages = read $ "[" ++ init (tail rest') ++ "]"
          lights = foldr (\x acc -> 2 * acc + if x == '.' then 0 else 1) 0 (init $ tail lightRaw)

parseInput :: String -> Input
parseInput = map parseLine . lines

combinations :: [a] -> [[a]]
combinations = sortOn length . go
    where go []       = [[]]
          go (x : xs) = go xs ++ map (x : ) (go xs)

fewestPresses :: Instruction -> Int
fewestPresses inst = case solution of
                        Nothing -> error "Something went wrong. Check your input."
                        Just xs -> length xs
    where target  = light inst
          presses = combinations
                  $ map (\x -> sum [2 ^ n | n <- x])
                  $ buttons inst
          solution = find ((== target) . foldl xor 0) $ tail presses

partOne :: Input -> Output
partOne = sum . map fewestPresses

optimizePresses :: Instruction -> IO SMTResult
optimizePresses inst = optLexicographic $ do
    ts <- sIntegers ["t" ++ show n | n <- [0 .. length (buttons inst) - 1]]
    for_ ts $ \t -> do
        constrain $ t .>= 0
    for_ (zip [0 ..] (joltage inst)) $ \(i, jolt) -> do
       constrain $ fromIntegral jolt .== sum [t | (t, b) <- zip ts (buttons inst), i `elem` b]
    minimize "total" (sum ts)

partTwo :: Input -> IO Output
partTwo input = do
    opts <- mapM optimizePresses input
    mins <- for opts $ \res -> do
                let x = fromJust $ getModelValue "total" res :: Int64
                return $ fromIntegral x
    return $ sum mins

day10 :: String -> String -> IO ()
day10 "parse" = print . parseInput
day10 "one"   = print . partOne . parseInput
day10 "two"   = print <=< (partTwo . parseInput)
day10 "sone"  = submit 2025 10 1 . show . partOne . parseInput
day10 "stwo"  = (submit 2025 10 2 . show) <=< (partTwo . parseInput)
day10 _       = error "Undefined part"
