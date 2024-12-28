module Day06.Main (day06) where

import           AOC           (submit)
import           Control.Monad
import           Data.Array.IO
import           Data.Char     (isDigit)
import           Data.Foldable (forM_)
import           Data.List     (isInfixOf)

data State = On
           | Off deriving (Show, Eq, Enum)

data InstrType = Toggle
               | TurnOff
               | TurnOn deriving (Show, Eq, Enum)

data Instruction = Instruction
                 { instructionType  :: InstrType
                 , instructionRange :: [(Int, Int)]
                 } deriving Show

toggleState :: State -> State
toggleState On  = Off
toggleState Off = On

type Input = [Instruction]
type Output = Int

parseInput :: String -> Input
parseInput = map parseInstruction . lines
    where parseInstruction :: String -> Instruction
          parseInstruction s
            | "on"     `isInfixOf` s = Instruction TurnOn  range
            | "off"    `isInfixOf` s = Instruction TurnOff range
            | otherwise              = Instruction Toggle  range
            where s'        = dropWhile (not . isDigit) s
                  [a, _, b] = words s'
                  (i, j)    = read ("(" ++ a ++ ")") :: (Int, Int)
                  (k, l)    = read ("(" ++ b ++ ")") :: (Int, Int)
                  range     = [(i', j') | i' <- [i .. k], j' <- [j .. l]]

partOne :: Input -> IO Output
partOne input = do
        arr <- newArray ((0, 0), (999, 999)) Off
            :: IO (IOArray (Int, Int) State)
        forM_ input $ \ (Instruction t r) -> do
            forM_ r $ \ pos -> do
                case t of
                    TurnOn  -> writeArray arr pos On
                    TurnOff -> writeArray arr pos Off
                    _       ->  readArray arr pos
                            >>= writeArray arr pos
                            .   toggleState
        elems <- getElems arr
        return (length . filter (== On) $ elems)

partTwo :: Input -> IO Output
partTwo input = do
        arr <- newArray ((0, 0), (999, 999)) 0 :: IO (IOArray (Int, Int) Int)
        forM_ input $ \ (Instruction t r) -> do
            forM_ r $ \ pos -> do
                cur <- readArray arr pos
                case t of
                    TurnOn  -> writeArray arr pos (cur + 1)
                    TurnOff -> writeArray arr pos (max 0 $ cur - 1)
                    _       -> writeArray arr pos (cur + 2)
        elems <- getElems arr
        return (sum elems)

day06 :: String -> String -> IO ()
day06 "parse" = print . parseInput
day06 "one"   = print <=< partOne . parseInput
day06 "two"   = print <=< partTwo . parseInput
day06 "sone"  = submit 2015 6 1 . show <=< partOne . parseInput
day06 "stwo"  = submit 2015 6 2 . show <=< partTwo . parseInput
day06 _       = error "Undefined part"
