module Main where

import           Data.Bits
import           Data.Char
import           Data.List
import           Data.Maybe
import           Numeric            (readInt)
import           System.Environment
import           System.IO
import           System.Process

data Operation   = AND | OR | XOR deriving (Eq, Read)
data Definition  = Definition { var :: String, val :: Int }
data Computation = Computation { operands :: [String], op :: Operation, res :: String }

instance Show Operation where
    show AND = ".&."
    show OR  = ".|."
    show XOR = "`xor`"

instance Show Definition where
    show d = var d ++ ":: Int\n" ++ var d ++ " = " ++ show (val d) ++ "\n"

instance Show Computation where
    show c = res c ++ " = Main." ++ head (operands c) ++ " " ++ show (op c) ++ " Main." ++ last (operands c) ++ "\n"

type Input = ([Definition], [Computation])
type Output = Int

readBin :: String -> Int
readBin bin = fst . head $ readInt 2 (`elem` "01") digitToInt bin

parseInput :: String -> Input
parseInput input = (map parseDef def, map parseOp ops)
    where (def, _ : ops) = break null $ lines input
          parseDef d = Definition (init a) (read b) where [a, b] = words d
          parseOp  o = Computation operands (read b) d
            where [a, b, c, _, d] = words o
                  operands        = sort [a, c]

partOne :: Input -> IO Output
partOne (defs, ops) = do
        (Just hin, Just hout, Just herr, _) <- createProcess (proc "runghc" []) { std_out = CreatePipe, std_in = CreatePipe, std_err = CreatePipe }
        hPutStrLn hin content'
        hClose hin
        res <- readBin <$> hGetLine hout
        hClose hout
        hClose herr
        return res
    where content  = "import Data.Bits\n\n" ++ concatMap show defs ++ "\n" ++ concatMap show ops
          zs       = sort [res op | op <- ops, head (res op) == 'z']
          zs'      = intercalate " ++ " ["show " ++ z | z <- reverse zs]
          content' = content ++ "\nmain = putStrLn (" ++ zs' ++ ")"

fixDubiousZ :: [Computation] -> String -> [String]
fixDubiousZ ops z@('z' : num) = [swap, z]
    where bits   = ['x' : num, 'y' : num]
          xori   = res . fromJust $ find (\c -> op c == XOR && operands c == bits)     ops
          lookup = find (\c -> op c == XOR && xori `elem` operands c) ops
          fixed  = head $ fixDubiousAndi ops xori                                                             -- In the case two swaps happened: zi - andi' or zi - couti and xori - andi
          swap | isNothing lookup = res . fromJust $ find (\c -> op c == XOR && fixed `elem` operands c) ops
               | otherwise        = res . fromJust $ lookup

fixDubiousAndi :: [Computation] -> String -> [String]
fixDubiousAndi ops andi = [swap, andi]
    where bits = operands . fromJust $ find (\c -> res c == andi)                     ops
          swap = res      . fromJust $ find (\c -> op c == XOR && operands c == bits) ops

partTwo :: Input -> String
partTwo (defs, ops) =  intercalate "," . map head . group . sort . concat $ swaps
    where hasOpWith o x   = any (\c -> op c == o && x `elem` operands c) ops
          fixDubiousAndi' = fixDubiousAndi ops
          fixDubiousZ'    = fixDubiousZ    ops

          xs  = [var d | d <- defs, head (var d) == 'x']
          ys  = [var d | d <- defs, head (var d) == 'y']
          xys = tail . map (\(x, y) -> [x, y]) $ zip xs ys

          andis           = [res o | o <- ops, op o == AND, operands o `elem` xys]
          dubiousAndis    = filter (hasOpWith XOR) andis
          dubiousZs       = [res o | o <- ops,  head (res o) == 'z', op o /= XOR, res o /= "z45"]
          swaps = map fixDubiousZ'    dubiousZs    ++
                  map fixDubiousAndi' dubiousAndis

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = partOne input >>= print
compute input "two"   = putStrLn . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args
