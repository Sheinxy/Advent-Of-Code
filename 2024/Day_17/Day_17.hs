module Main where

import           Data.Array.IArray
import           Data.Bits
import           Data.List
import           Data.Maybe
import           System.Environment

data Computer = Computer { program :: Array Int Int, pc :: Int, regA :: Int, regB :: Int, regC :: Int, stdout :: [Int]} deriving Show

type Input = Computer
type Output = Int

parseInput :: String -> Input
parseInput input = Computer (parseProgLine p) 0 (parseRegLine a) (parseRegLine b) (parseRegLine c) []
    where [a, b, c, _, p] = lines input
          parseRegLine regLine = read num
            where _ : _ : [num] = words regLine
          parseProgLine progLine = listArray (0, length ops - 1) ops
            where _ : [opsStr] = words progLine
                  ops          = read $ "[" ++ opsStr ++ "]"

comboOperand :: Int -> Computer -> Int
comboOperand n | n <= 3 = const n
               | n == 7 = error "Invalid operand"
comboOperand 4 = regA
comboOperand 5 = regB
comboOperand 6 = regC

adv :: Computer -> Computer
adv computer = computer { regA = res, pc = pcounter + 1 }
    where pcounter = pc computer
          operand  = program computer ! pcounter
          n   = regA computer
          d   = comboOperand operand computer
          res = n `div` (2 ^ d)
bxl :: Computer -> Computer
bxl computer = computer { regB = res, pc = pcounter + 1 }
    where pcounter = pc computer
          operand  = program computer ! pcounter
          b   = regB computer
          res = b `xor` operand

bst :: Computer -> Computer
bst computer = computer { regB = res, pc = pcounter + 1 }
    where pcounter = pc computer
          operand  = program computer ! pcounter
          res      = comboOperand operand computer `mod` 8

jnz :: Computer -> Computer
jnz computer = computer { pc = pcounter' }
    where pcounter = pc computer
          operand  = program computer ! pcounter
          pcounter' | regA computer == 0 = pcounter + 1
                    | otherwise          = operand

bxc :: Computer -> Computer
bxc computer = computer { regB = res, pc = pcounter + 1 }
    where pcounter = pc computer
          b = regB computer
          c = regC computer
          res = b `xor` c

out :: Computer -> Computer
out computer = computer { stdout = res : stdout computer, pc = pcounter + 1}
    where pcounter = pc computer
          operand  = program computer ! pcounter
          res = comboOperand operand computer `mod` 8


bdv :: Computer -> Computer
bdv computer = computer { regB = res, pc = pcounter + 1 }
    where pcounter = pc computer
          operand  = program computer ! pcounter
          n   = regA computer
          d   = comboOperand operand computer
          res = n `div` (2 ^ d)

cdv :: Computer -> Computer
cdv computer = computer { regC = res, pc = pcounter + 1 }
    where pcounter = pc computer
          operand  = program computer ! pcounter
          n   = regA computer
          d   = comboOperand operand computer
          res = n `div` (2 ^ d)

getInstruction :: Int -> Computer -> Computer
getInstruction 0 = adv
getInstruction 1 = bxl
getInstruction 2 = bst
getInstruction 3 = jnz
getInstruction 4 = bxc
getInstruction 5 = out
getInstruction 6 = bdv
getInstruction 7 = cdv

runInstruction :: Computer -> Computer
runInstruction computer = instruction computer'
    where pcounter    = pc computer
          opcode      = program computer ! pcounter
          instruction = getInstruction opcode
          computer'   = computer { pc = pcounter + 1 }

runInstructions :: Computer -> Computer
runInstructions = head . filter isDone . iterate runInstruction
    where isDone computer = pc computer > (snd . bounds) (program computer)

partOne :: Input -> String
partOne = intercalate "," . map show . reverse . stdout . runInstructions

partTwo :: Input -> Int
partTwo computer = fromJust $ findValue target 0
    where target = reverse . elems $ program computer
          tryValue value = last . stdout $ runInstructions (computer { regA = value })
          findValue [] acc = Just acc
          findValue (t : ts) acc
                | null candidates = Nothing
                | otherwise       = head $ filter isJust [findValue ts candidate | candidate <- candidates]
            where values     = [acc * 2 ^ 3 .. acc * 2 ^ 3 + 7]
                  candidates = map fst . filter ((t ==) . snd) $ zip values (map tryValue values)

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = putStrLn . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args
