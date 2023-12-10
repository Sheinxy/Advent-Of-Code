module Main where

import Data.Matrix (Matrix, (!), fromLists, toLists, ncols, nrows, safeGet, setElem)
import Data.Maybe
import Data.Set (Set, insert, empty, notMember, size, disjoint, toList, fromList, singleton)
import qualified Data.Set as S (filter)
import Data.Tuple.Extra ((&&&), both)
import System.Environment

data Heading = East | West | North | South deriving (Eq)

type Input = ((Int, Int), Matrix Char)
type Output = Int

parseInput :: String -> Input
parseInput = starting &&& (fromLists . lines)
    where starting input = head [(i, j) | (i, l) <- zip [1 .. ] $ lines input,
                                          (j, c) <- zip [1 .. ] l, c == 'S']

-- Is the tile in that direction preventing from going on it?
blockUp    = (`elem` "JL-.")
blockDown  = (`elem` "7F-.")
blockLeft  = (`elem` "7J|.")
blockRight = (`elem` "FL|.")

-- Get a tile from the Matrix, get '.' is not inside
getWithDefault :: Matrix Char -> (Int, Int) -> Char
getWithDefault grid (r, c) | isJust m  = fromJust m
                           | otherwise = '.'
                           where m = safeGet r c grid

-- Get a pipe's neighbours
getNeighbours :: (Int, Int) -> Matrix Char -> [(Int, Int)]
getNeighbours pos@(r, c) grid = filter ((/= '.') . (grid !)) neighbours
    where rows = nrows grid
          cols = ncols grid
          char = grid ! pos
          inGrid (row, col) = 0 < row && row <= rows && 0 < col && col <= cols
          -- S is a bit peculiar: we need to make sure that its neighbours are actually accessible
          neighboursOf 'S'  = map snd . filter (not . fst) $ zipWith (\f v -> (f $ getWithDefault grid v, v)) [blockUp, blockDown, blockLeft, blockRight] 
                                                                                                              [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]
          neighboursOf 'F'  = [(r + 1, c), (r, c + 1)]
          neighboursOf '7'  = [(r + 1, c), (r, c - 1)]
          neighboursOf 'J'  = [(r - 1, c), (r, c - 1)]
          neighboursOf 'L'  = [(r - 1, c), (r, c + 1)]
          neighboursOf '-'  = [(r, c - 1), (r, c + 1)]
          neighboursOf '|'  = [(r - 1, c), (r + 1, c)]
          neighbours        = filter inGrid . neighboursOf $ char

-- Simple DFS traversal to get the loop
getLoop :: Input -> [(Int, Int)]
getLoop (s, g) = dfs empty (-1, -1) s
    where dfs v prev pos | s `elem` nexts     = [pos]                                       -- If the starting point is in the next positions, then we found the loop, and the current pos is part of it
                         | not (all null res) = pos : (head . filter (not . null) $ res)    -- If one of the next positions gives a result, then it means that we can join the loop from the current position
                         | otherwise          = []                                          -- If no next position gives a result, then this is not part of the loop
                         where nexts = filter (/= prev) . filter (`notMember` v) $ getNeighbours pos g -- The next positions are the non-visited neighbours. Prev is only useful to make sure we're not directly coming from the start
                               res   = map (\n -> dfs (insert n v) pos n) nexts                        -- We continue the dfs on each next position
                           
partOne :: Input -> Output
partOne = (`div` 2) . length . getLoop -- The furthest point from the start is at half the loop

-- Get the type of tile coresponding to the starting point
getType :: Matrix Char -> (Int, Int) -> Char
getType grid (r, c) = typeOf . zipWith ($) [blockUp, blockDown, blockLeft, blockRight] $ map (getWithDefault grid) [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]
    where typeOf [True, True, _, _] = '-'
          typeOf [True, _, True, _] = 'F'
          typeOf [True, _, _, True] = '7'
          typeOf [_, _, True, True] = '|'
          typeOf [_, True, True, _] = 'L'
          typeOf [_, True, _, True] = 'J'

-- Get the neighbours of the position that are not part of the loop (assuming that the grid has already been stipped of non-loop tiles)
getNeighboursOutOfLoop :: (Int, Int) -> Matrix Char -> [(Int, Int)]
getNeighboursOutOfLoop pos@(r, c) grid = filter ((== '.') . getWithDefault grid) neighbours
    where rows = nrows grid
          cols = ncols grid
          inWorld (row, col) = 0 <= row && row <= rows  + 1 && 0 <= col && col <= cols + 1
          neighbours        = filter inWorld [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

-- Follow the loop to get the bordering tiles in the form of (Left bordering tiles, Right bordering tiles): imagine you start at a straight line, you go east, and you keep record of everything on your left and on your right.
followLoop :: Matrix Char -> Set (Int, Int) -> (Set (Int, Int), Set (Int, Int))
followLoop grid loop | null straights = (empty, empty)                                          -- If there is (somehow) no '-' tile, then there cannot be any loop with enclosed parts.
                     | otherwise      = both (S.filter ((== '.') . getWithDefault grid)) border -- Only keep the borders that are not part of the loop (so for example .||, the middle tile only as the left .)
    where straights     = S.filter ((== '-') . (grid !)) loop
          line@(r, c)   = head . toList $ straights
          (left, right) = go (r, c + 1) (grid ! (r, c + 1)) East
          border        = (insert (r - 1, c) left, insert (r + 1, c) right)
          -- Turning pipes borders
          borders (r, c) 'F' = [(r, c - 1), (r - 1, c)]
          borders (r, c) '7' = [(r, c + 1), (r - 1, c)]
          borders (r, c) 'L' = [(r, c - 1), (r + 1, c)]
          borders (r, c) 'J' = [(r, c + 1), (r + 1, c)]
          -- Simple pipes
          go p '-' _ | p == line   = (empty, empty)
          go (r, c) '-' East  = (insert (r - 1, c) left, insert (r + 1, c) right) where (left, right) = go (r, c + 1) (grid ! (r, c + 1)) East
          go (r, c) '-' West  = (insert (r + 1, c) left, insert (r - 1, c) right) where (left, right) = go (r, c - 1) (grid ! (r, c - 1)) West
          go (r, c) '|' North = (insert (r, c - 1) left, insert (r, c + 1) right) where (left, right) = go (r - 1, c) (grid ! (r - 1, c)) North
          go (r, c) '|' South = (insert (r, c + 1) left, insert (r, c - 1) right) where (left, right) = go (r + 1, c) (grid ! (r + 1, c)) South
          -- Turning pipes
          go p@(r, c) 'F' North = (foldr insert left $ borders p 'F', right) where (left, right) = go (r, c + 1) (grid ! (r, c + 1)) East
          go p@(r, c) 'F' West  = (left, foldr insert right $ borders p 'F') where (left, right) = go (r + 1, c) (grid ! (r + 1, c)) South
          go p@(r, c) '7' North = (left, foldr insert right $ borders p '7') where (left, right) = go (r, c - 1) (grid ! (r, c - 1)) West
          go p@(r, c) '7' East  = (foldr insert left $ borders p '7', right) where (left, right) = go (r + 1, c) (grid ! (r + 1, c)) South
          go p@(r, c) 'L' South = (left, foldr insert right $ borders p 'L') where (left, right) = go (r, c + 1) (grid ! (r, c + 1)) East
          go p@(r, c) 'L' West  = (foldr insert left $ borders p 'L', right) where (left, right) = go (r - 1, c) (grid ! (r - 1, c)) North
          go p@(r, c) 'J' South = (foldr insert left $ borders p 'J', right) where (left, right) = go (r, c - 1) (grid ! (r, c - 1)) West
          go p@(r, c) 'J' East  = (left, foldr insert right $ borders p 'J') where (left, right) = go (r - 1, c) (grid ! (r - 1, c)) North

-- Oh god, I overcomplicated stuff once again :)
-- The simple solution would simply be to see how many walls are separating my tile from the border.
-- Oh well: Get the enclosed part of the loop, also returning the new grid for my bonus round
getInsideOfLoop :: Input -> (Set (Int, Int), Matrix Char)
getInsideOfLoop input@(s, g) = (inside, g'')
    where loop       = fromList $ getLoop input
          g'         = setElem (getType g s) s g -- Changing S to its actual type
          g''        = foldr (setElem '.') g' [(i, j) | i <- [1 .. nrows g], j <- [1 .. ncols g], g' ! (i, j) /= '.', (i, j) `notMember` loop] -- Stripping non loop elements from the grid (replacing them by .)
          out        = bfs (singleton (0, 0)) [(0, 0)] -- Starting from (0, 0), which is outside the grid, recovering the outside part of the loop that doesn't require squeezing through pipes.
          (l, r)     = followLoop g'' loop             -- Follow the loop to get the left and right borders. One is part of the inside, the other is part of the outside.
          insideBorder | disjoint out l = toList l     -- If no element from the outside is present on the left border, then the left border is part of the inside
                       | otherwise      = toList r     -- Otherwise it is the right border
          inside     = bfs (fromList insideBorder) insideBorder -- Now move around from the inside points to get the inside part of the loop
          bfs v []         = v
          bfs v (el:queue) = bfs v' queue'
            where neighbours = filter (`notMember` v) $ getNeighboursOutOfLoop el g''
                  v'         = foldr  insert v neighbours
                  queue'     = queue ++ neighbours

partTwo :: Input -> Output
partTwo = size . fst . getInsideOfLoop

-- Bonus round: print the map with O for the outside part of the loop and I for the enclosed part :)
bonusRound :: Input -> IO ()
bonusRound input = putStr g''
    where (inside, g) = getInsideOfLoop input
          g'          = foldr (setElem 'I') g $ toList inside
          g''         = concatMap ((++ "\n") . map go) $ toLists g'
          go '.'      = 'O'
          go x        = x

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print . partTwo $ input
compute input "bonus" = bonusRound input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input)  $ init args 
