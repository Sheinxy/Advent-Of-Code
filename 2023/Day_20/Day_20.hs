module Main where

import Data.Map (Map, fromList, (!), adjust, elems, empty, keys, insert, member, notMember)
import Data.List (find, intercalate, stripPrefix)
import Data.Maybe (fromJust)
import System.Environment

data State  = On   | Off deriving (Show, Eq)
data Pulse  = High | Low deriving (Show, Eq)
data Module = Broadcaster { connected :: [String] }         |
              Flip { state :: State, connected :: [String]} |
              Conjunction { inputs :: Map String Pulse, connected :: [String] } deriving (Show, Eq)

data Iteration = Iteration { modules :: Input, presses :: Int, cycling :: Map String Int, lows :: Int, highs :: Int }
                    deriving (Show, Eq)

type Input = Map String Module
type Output = Int

isConjunction :: Module -> Bool
isConjunction (Conjunction _ _) = True
isConjunction _                 = False

parseInput :: String -> Input
parseInput input  = foldr getInputs modules $ keys modules
    where modules = fromList . map (parseLine . words) . lines $ input
          getInputs name ms = foldr (adjust (addInput name) ) ms [child | child <- connected (ms ! name),
                                                                                             child `member` ms,
                                                                                             isConjunction  (ms ! child)]
          addInput input m = m { inputs=insert input Low $ inputs m}
          parseLine (name : _ : xs) | head name == '&' = (tail name, Conjunction { inputs=empty, connected=children } )
                                    | head name == '%' = (tail name, Flip        {  state=Off  , connected=children } )
                                    | otherwise        = (    name , Broadcaster {               connected=children } )
                                    where children = map (filter (/= ',')) xs

doIteration :: [(String, String, Pulse)] -> Iteration -> Iteration
doIteration [] it = it

doIteration [(_, "broadcaster", Low)] it = doIteration [("broadcaster", c, Low) | c <- children] (it { lows=l' })
                                              where children = connected $ modules it ! "broadcaster"
                                                    l'       = lows it + length children

doIteration ((sender, name, pulse):queue)  it | name `notMember` modules it            = doIteration queue it
                                              | pulse == High && not (isConjunction m) = doIteration queue it { cycling = c' }
                                              | otherwise                              = doIteration queue' it'
                                              where m = modules it ! name
                                                    -- Update cycle length of inputs of conjunction module connected to rx
                                                    c' | pulse /= High || "rx" `notElem` connected m = cycling it
                                                       | otherwise = insert sender (presses it) (cycling it)

                                                    -- Update the module
                                                    m' | isConjunction m = m { inputs = adjust (const pulse) sender (inputs m) }
                                                       | state m == On   = m { state = Off }
                                                       | otherwise       = m { state = On  }

                                                    -- Get the next pulse to send
                                                    getPulse (Conjunction ins _) | all (== High) $ elems ins = Low
                                                                                 | otherwise                 = High
                                                    getPulse (Flip On _)         = High
                                                    getPulse (Flip Off _)        = Low
                                                    p' = getPulse m'

                                                    -- Add this pulse to the send queue
                                                    queue' = queue ++ [(name, child, p') | child <- connected m']

                                                    -- Update the iteration: modules and counters
                                                    l'     | p' == High = lows  it
                                                           | otherwise  = lows  it + length (connected m')
                                                    h'     | p' == Low  = highs it
                                                           | otherwise  = highs it + length (connected m')
                                                    mods'  = adjust (const m') name $ modules it
                                                    it'    = it { modules = mods', cycling = c', lows = l', highs = h' }

doNextIteration :: Iteration -> Iteration
doNextIteration (Iteration it n c _ _) = doIteration [("button", "broadcaster", Low)] (Iteration it (n + 1) c 1 0)

doNIterations :: Int -> Input -> [Iteration]
doNIterations n input = take n . tail . iterate doNextIteration $ Iteration input 0 empty 0 0

partOne :: Input -> Output
partOne input = (sum . map lows) iterations * (sum . map highs) iterations where iterations = doNIterations 1000 input

partTwo :: Input -> Output
partTwo input = foldr1 lcm . elems $ cycling iterations
    where mustBeHigh = keys . inputs . fromJust . find (("rx" `elem`) . connected) . elems $ input
          allFound it = all (`elem` (keys $ cycling it)) mustBeHigh
          iterations  = head . dropWhile (not . allFound) . tail . iterate doNextIteration $ Iteration input 0 empty 0 0

graphOfModules :: Map String Module -> String
graphOfModules modules = "digraph {\n\trx [fillcolor=fuchsia style=filled shape=box];\n" ++ go (keys modules) ++ "}"
    where go []   = ""
          go (x:xs) = nodeStr ++ go xs
            where nodeStr | Broadcaster c <- modules ! x = "\tbroadcaster [fillcolor=gold2 style=filled shape=diamond];\n\tbroadcaster -> {" ++ intercalate " " c ++ "};\n"
                          | Flip On     c <- modules ! x = "\t" ++ x ++ " [fillcolor=green style=filled shape=ellipse];\n\t" ++ x ++ " -> {" ++ intercalate " " c ++ "};\n"
                          | Flip Off    c <- modules ! x = "\t" ++ x ++ " [fillcolor=red   style=filled shape=ellipse];\n\t" ++ x ++ " -> {" ++ intercalate " " c ++ "};\n"
                          | Conjunction a c <- modules ! x  = str a c 
                            where str a c | all (== High) (elems a) = "\t" ++ x ++ " [fillcolor=red     style=filled shape=hexagon];\n\t" ++ x ++ " -> {" ++ intercalate " " c ++ "};\n"
                                          | otherwise               = "\t" ++ x ++ " [fillcolor=green   style=filled shape=hexagon];\n\t" ++ x ++ " -> {" ++ intercalate " " c ++ "};\n"


compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input  arg    | Just n <- stripPrefix "graph=" arg = putStrLn . graphOfModules . modules . last . doNIterations (read n) $ input
compute input "graph" = putStrLn . graphOfModules $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args
