module Main where

import Data.Map (Map, fromList, (!), adjust, elems)
import Data.List.Extra (replace)
import Data.Tuple.Extra ((***))
import System.Environment

data Ratings   = Ratings   { x :: Int, m :: Int, a :: Int, s :: Int    } deriving (Read)
data Rule      = Rule      { condition :: Condition, ifTrue  :: String }
data Condition = Condition { operation :: Int -> Bool, field :: Char   }

type Input  = (Map String [Rule], [Ratings])
type Output = Int

parseInput :: String -> Input
parseInput = ((fromList . map parseWorkflow) *** (map parseRatings . tail)) . break null . lines
    where parseRatings   = read . ("Ratings " ++)
          parseWorkflow  = (\(name:rules) -> (name, map parseRule rules)) . words .
                                                                            replace "}" " " .
                                                                            replace "{" " " .
                                                                            replace "," " "
          parseRule rule | not . null $ ifTrue = Rule (parseCond cond) $ tail ifTrue    -- If there is a condition: get the op
                         | otherwise           = Rule (Condition (const True) 'x') cond -- If there is no condition (last rule): always return true
                          where (cond, ifTrue) = break (`elem` ":") rule
          parseCond cond = Condition (getOp op num) rating
                          where ([rating], op:num) = break (`elem` "<>") cond
                                getOp '<' num = (< read num)
                                getOp '>' num = (> read num)

getField :: Char -> (Ratings -> Int)
getField 'a'  = a
getField 'm'  = m
getField 's'  = s
getField 'x'  = x

getNextState :: Ratings -> [Rule] -> String
getNextState ratings = ifTrue . head . dropWhile (not . checkRule)
    where checkRule (Rule (Condition op rating) _) = (op . getField rating) ratings

getAcceptation :: Map String [Rule] -> Ratings -> String
getAcceptation workflows ratings = head . dropWhile (`notElem` ["A", "R"]) .
                                   iterate (getNextState ratings . (workflows !)) $ "in"

partOne :: Input -> Output
partOne (workflows, ratings) = sum . map getRatingsValue . filter (("A" ==) . getAcceptation workflows) $ ratings
    where getRatingsValue ratings = sum . map ($ ratings) $ [a, m, s, x]

-- Returns a list of Map. Each element of the list represent combinations leading to an accepting state
followPathsToAcceptance :: Map String [Rule] -> [Map Char [Int]]
followPathsToAcceptance workflows = go "in" . fromList . map (, [1 .. 4000]) $ "amsx"
    where go "A" ranges = [ranges]                           -- This is an accepting state: yield the accepting numbers
          go "R" _      = [      ]                           -- This is a rejecting state: yield no number
          go _  ranges  | all null . elems $ ranges = []     -- This is an impossible state: yield no number
          go cur ranges = tryPaths (workflows ! cur) ranges  -- For a regular state, follow every possible path
            where tryPaths [] _            = []              -- If there is no path left to follow, stop the loop
                  -- Try to follow the current rule. Try to reject it. Combine the two results:
                  tryPaths (r : rs) ranges = go (ifTrue r) trueRanges ++ tryPaths rs falseRanges
                      where cond        = condition r
                            op          = operation cond
                            rating      = field cond
                            trueRanges  = adjust (filter        op)  rating ranges -- The numbers that allow to follow the rule
                            falseRanges = adjust (filter (not . op)) rating ranges -- The numbers that allow to reject the rule

partTwo :: Input -> Output
partTwo = sum .
          map (product . map length . elems) .
          followPathsToAcceptance .
          fst

compute :: Input -> String -> IO ()
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args
