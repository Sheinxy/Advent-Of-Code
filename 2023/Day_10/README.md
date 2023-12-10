## Day 10

Oh boy... I overcomplicated things again 😽

```hs
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
          isAccessible  f p = (f $ getWithDefault grid p, p)
          getAccessible fs  = map snd . filter (not . fst) . zipWith isAccessible fs
          neighboursOf 'S'  = getAccessible [blockUp  , blockDown, blockLeft, blockRight] [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]
          neighboursOf 'F'  = getAccessible [blockDown, blockRight] [(r + 1, c), (r, c + 1)]
          neighboursOf '7'  = getAccessible [blockDown, blockLeft ] [(r + 1, c), (r, c - 1)]
          neighboursOf 'J'  = getAccessible [blockUp  , blockLeft ] [(r - 1, c), (r, c - 1)]
          neighboursOf 'L'  = getAccessible [blockUp  , blockRight] [(r - 1, c), (r, c + 1)]
          neighboursOf '-'  = getAccessible [blockLeft, blockRight] [(r, c - 1), (r, c + 1)]
          neighboursOf '|'  = getAccessible [blockUp  , blockDown ] [(r - 1, c), (r + 1, c)]
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
```

Yikes! That's a lot of code!

### AND I WON'T EXPLAIN IT.

Well, to be honest, I will explain my reasoning. But unlike every day so far, I won't analyze my code function by function, as it would be a bit too tedious.
I left comments in my code, so if you're brave enough to read what I wrote (there's A LOT of code here, yikes), hopefully they will help you.

Anyway, let's start explaining:

## Part 1
---

This was the easy part. I start at the starting tile, and I perform a [Depth First Search](https://en.wikipedia.org/wiki/Depth-first_search).
To know my next positions, I use a function called getNeighbours which, for a given pipe tile will get the accessible neighbouring pipe tiles. (For example, if I have "7-J", and I'm looking for the neighbours of the middle tile, then I will get the "J" tile only, as this is the "7" tile isn't accessible).
The stop case for my dfs is when I find the starting point again. This means that my current tile is part of the loop. If one of the neighbours of my current tile has yield a non empty result (i.e it is part of the loop), then my current tile is also part of the loop.

With that, I am able to get the loop. I made a small bonus round in my code that allows me to print my loop!

So for example, if the input was:
```
-L|F7
7S-7|
L|7||
-L-J|
L|-JF
```

I would find the following loop:
```
.....
.F-7.
.|.|.
.L-J.
.....
```

Note that my code is able to replace the S with its actual tile! This is not done for part one, but I use it in part 2 😸

Now, with my actual input:
```
F-|-J7F7F7F-7F7F--7-L|-L7777.FFL-|.|.FF-FF7FFFFF7--F-J7.|-FF77|-F|777-F7.F7FF|-J7|-7.FFJ77FL.7-|-|7.|.77|-F-|-F|-FL.F77.FF-77|7.7--7--7FJ--7
|7LJ7.F-JLJ7L7--7F77.|LLJ|LF7|.LLJ-|7L|FJLF77L-FJ7FJ.LFF|7L-LLF-LJF777||7L-F7LLLLJ7|L7||F-J|-|.J.F7FF7F7|J|F|-FJLF.FLL|7J|FF-7-FJ7J|J.F7L|LJ
L|J.7.L-J7FJ--..F-J|-7-|.J|LL-J.F|J|-77L|.LL7J||FJ7|.F--J7-J|7.LJJJJFFJL7JF||7.||-LJ-F7-|FFJ-|77.|L7||||7F-LJF|L7J7FF|JL7L7JJL-|---|7-JJ-FJ7
LJ.L-|--JF7J|LF7|L-7.|FFJJ-.|JL7JF7F7F7777F-JFFJJLLJFL-J-|.FLJ-7|FF7FL7FJF7||F7FF-J7F||F7-7L7L|7.L7||LJ|.LJ|FFJFF7F7FLLJLJ||LL-J7.L|7LJJFJF-
.L7J7L7JJF|-||||7LJ|LFL7J-LL|7-|-.F|LJ|F7F7JF|7J7.FFJ|-JF-7F7.F77FJ|F7||FJ||LJL-7.FFFJ|||F|7|-JFF7|||F7|7--J||.JJFFF7J7.JJL-.L7L---LF7|J|-|J
.||||-7FF---J--LJ-F|F|JL|.|.|.F||.-L-7LJLJL7-JJJ--F7FF7-|LFJL-JL7L7||||||FJL-7F-JFJJL7LJL7F--7JFJLJLJ|LJJ-FF|--J.J-L-7F7J.L7|.J.F||FJ--.|.L-
.-7-J.L7J7.F|JLLJFL--L7FLFL---FJ-7L||L7F---JJ-J.||||FJ|F-7|F---7L-J||||LJL--7|L--7LFFL7F-J|F-J.L--7F-JF77.F.|FF-LJL|LL7|.FL|LJ--7-7J|..----J
|JJJLF.L.--|-FJ7..LLF-7FL7|LJF7J-F-7F7|L--7J.F.FFFJ||FJL7|LJF--JF-7LJ||F----J|F--JF7F-J|JFJ|||-F7FJ|7L||7-J-F-|7|J|LF-JLF-7|-7J.F-J-|F|J.L|.
|J||7L-J.|FLF-FJJ-7LJF|J.J|-.7|F-L7LJLJF--JF-7-||L7|||F7||F7L7F-JJL-7LJL-7FF7||F--J|L-7|FJFJFF-J||FJ7L|L7.L7L7.F|.L-F-|7L-JJ.F7--7L-|-|-7.|F
|--7LJ.|FF7F--JJJ.F.FLJL|-|JFF-|J7L---7L-7||FJJLLFJLJLJLJLJ|-LJF7.F-JF---JFJLJLJF7FJF7||L7|F7L-7||L7F7L7|7FJ77-F7-|J|FF-JJ|7FF--J|.L7FL7J.|J
FJFJ|.|L--L-LFJJ.L7-JF|LL.L.7JLJ.FFF7FJF-JFJL7JFLL-7F7F7F-7L7F-JL-JF7L7F7-L7F---JLJFJ|||FJLJL7FJLJFJ|L-JL7J7.|-F|F7|LFJ||J|FF7.F7J77JFF|F-J|
|.JJ|FFJ.FL7L7J|FL-.FJJ|F--7-.J..LFJ|L7L7FJF-J.7JF-J|LJ|L7L-JL---7FJL7LJ|F7|L-7F7F7L7||LJF---JL7F-J|L7F7FJJFF|-JL7L|J|F-JF--L|F7|LF|7J||7.F7
L|-F-FJ7-7.L7L7F7-LJ7..-JJ-F7JFF|.|FJL|FJL7||F77.L--JF-JFJ7F7.F--J|F7L-7LJLJF-J|LJ|FJ||F-J.F7F7|L-7F-J|LJF77.||--L-L.LJJL|FL|L777.LJJ-JJ7-||
.LFJLJJ7FL7J|FJ-L77|JF-L7F7.F7FF7FJL7FJL7FJL7|L-7F7F7L7FJF7|L7L---J||F-JF---JF-JF7||FJ|L--7|||||F-JL-7L--JL7-L7.LF.L|.|.FLL7J-LL-7LJLFJLF.L|
FJJ7F---7J||JLLLJ|FJ7.LJ77FFF-7||L7FJL-7||F7||F-J||||FJL-J||FJF----J|L-7|F7F7L--J||||FJF--J|LJLJL7F7FJF----JJ||7.F77F.F-L.|7J7LJ.|.|LL|LLJ||
77.FLJ|.F.FL7JJ|FLL7F|FL|JL-L7LJL-J|F7FJLJ||LJL7FJ||LJF---J||.L7F7F7L7FJ||LJL--7F||||L7L7F7L-7F-7|||L7L----7F7-F7||FJ.7.J-7|L7.|F7F-..|LLLLL
|7--JLL---|J.L---7|F-JF-7F7..L----7|||L-7FJL---JL7|L-7|F7JL||F7LJLJL7|L7||F----JFJLJ|FJFJ|L-7||F||||FJF----J||F7FJL7JF77|F-J-.--J.|-J7JF-FJJ
.JLL|LL-JF|F77...|J-|7L-JFL7.F---7|LJ|F7|L-7F7F7FJL7FJ||L7FJLJ|F7F7FJL-JLJL7F7F7L--7|L7|FJF7LJL7LJ||L7|.F7F7|||LJF-J.|L7JJ.J-F-|F|J77L-JF|.|
F|-JL7.FF77|F7-LL.LL|F-JLF--FL7F7LJF-J|||F-J||||L7FJL7||FJ|F--J||||L--7F7F7LJ|||F--JL-J||FJL-7FJF-J|7|L-J||LJ|L-7L---JFJ.|-L7--LJLFF||..|LJJ
FJ7FJ|F--77|.L7J.-7LJLF.LF77LFJ|L-7L7FJ||L-7||||J||F-J||L7|L7F7|||L7F-J|LJ|F-J||L-----7LJL-7FJ|FJF7L7|F--J|F-JF-JF7F7FJ7-F7J|.F|JL-L7FL-J-||
LJFJLL7L||L77L7..F|7-F--7||F7L-JF7|FJL7LJF-J||||FJ|L-7||FJL7||||LJFJL--JF7||F7|L7JF7F-JF---JL-JL-JL7|||F7FJL7|L7FJLJLJF-7||LF-777..F|F|7|J|J
.FJ7-FJ.7-|F-JLF-FF7.L-7|||||-F-JLJL--JF7|F7||||L7L7FJLJL7FJLJ|L-7|F7F-7|LJ||LJFJFJLJF7||F7F7F7FF7FJLJ||LJF-JF7||F7F-7|FJ|L7L7|LJ7F7FLLJJ-|.
F.||-J|.|LFL7.FF-J||F7F|LJLJL7L--7F7F-7|LJ|||LJL7|FJL7F--JL7F7|F-JLJ|L7|L-7LJF7L7L---J|L-JLJ||L7||L-7FJ|F-JF7|||LJLJFJ||FJFJFJL7.F--7FFJ|LL7
L7|-7FL-77.FL-F-7FJLJL-JF7F-7L7F7LJLJFJ|F-J||F--J|L7FJL7-F7||LJL--7FJFJL--JF-JL-J|F---JF7F--J|FJ|L7FJL-JL-7|||LJF--7L7||L7L7|F7|JL|F|7|77||F
FLJFLLLFFJ7|7L||FJF-----JLJ.|FJ||F7F7L7LJF7LJ|F--JFJL7FJFJ||L7-F7FJL7L-7F-7L7F---7L----J|L7F7|L7L7|L7F7F--J|||F-JF7L-J||7L7|||LJJF|-JF|-JFJ-
F|7|7J.||77LJ--LL-J-F7F--7F-J|FJLJ|||7L7FJ|F-JL--7|F7||||FJL7L7||L-7L--JL7L7||F--JF7F---JFJ||L7|FJL-J|LJF7J|LJ|F-JL--7||F7|LJL--7J.|LJ.FF-JJ
FJFJ|7-.F77J-7FLF7LFJ|L-7|L-7|L--7LJ|F7||L|L-7F-7|||LJL7||F7L7||L77L---7FJJ|LJL-7F||L---7L-J|FJ|L---7L77|L7L-7LJF----J|||||F----JJ-J.|-L--F7
||-7|7-LLJ7.-JFFJL7L7|F7||F-JL7F7L-7||LJL7L--J|FJ||L-7FJ||||7|||FJF7F7FLJF7|F---JFJ|F7F7|F--J|FJF--7L7L-JFJF7|F-JJF--7|LJ|||F7F77|JFF.|J-LL7
7J7F|LFLJJJ.FF-JF7L7||||||L-7FJ|L7FJ||F--JF7F7||FJL7FJL-JLJL7|LJL7|LJ|F7FJLJL---7|FJ|LJ|||.F7|L7|F-JFJF--JFJLJ|FF7|F7|L-7LJLJLJ|F77|.7--7-|J
|.-LJF|FJ||L-|F-JL7|||||||F7|L-JFJL7|||F-7|||LJ||F-JL-7F----JL-7FJL-7||LJF-7F7F7||L7L-7LJL7|||FJ|L-7L7L7F7|F--JFJ|||LJF7|F7F--7LJL7JF-7-|J||
-7JF7-JJ.F77LLJF--JLJ||||||LJF-7L--J||||FJ||L-7|||F7|FJ|F7F7LF-JL7JFJ||F-JJ||LJLJ|FJF7L7F-J|LJL7|F7|FJFJ||||JF-JFJ|L-7||LJ||F7L---JL-||-77.-
||.L.-7||F-JJ|.L----7|||||L-7|JL---7||||L7|L-7|||LJL7L7||||L7L-7FJFJFJLJF7FJL---7||FJ|FJL-7L-7FJLJ||L7|FJLJL7|F-JL|F-J|L7F|LJL-7F--7-||.|J77
F77.L-|-LL.|-FF-----JLJLJ|F7LJF7F7J||||L7||F-J||L--7L-JLJ||FJF-JL-JFJF7FJLJF----J||L7|L7F-JF7|L-7L||FJLJF---J||LF-JL-7L7|FJF7F-J|F-J7L7FJ.LF
L-F.L7|JL|7FF7L--7F7F-7F7LJL--JLJ|FJLJL7|LJL7FJL-7LL-7F--J|L7L----7L7||L--7L-7F7FJL7||FJL-7|||F-JFJ|L-7FJF7F7|L-JF---JFJLJFJ|L--JL7-7.-|.F|J
FLL7L-|7F--FJL--7LJLJ7LJL7F7F---7|L---7|L-7FJ|F--JF77||-F7|FJF7-F7L7|||F7||F-J||L7FJ||L7F-J||||F7L7|F7||FJLJLJF--JF7F7|F7FJ.L7F-7FJF|7|F.F7J
F7FL7L|--7FL---7L7F-7F---J|LJF--JL7F-7|L7FJ|FJ|F7FJL7|L7|||L7|L7|L7|LJ|||FJ|F7||FJL7||FJ|F7|||||L7|LJ||||F---7L7F-J|||||LJF7|||FLJF7F77J.LJ7
L-|-L7JL7FJ7LJFL7LJFJL----JF7L7F-7LJ-|L7||FJ|FJ|||F-JL7||LJ|||-||FJ|F-J|||-LJ||||F-J|||-||||||||.|L7FJ||||F7|L7LJF-J|LJL7FJL7|L7F7|LJ||.F-LF
JL|.L7FL7JF||LF7L-7|F7JF---JL-J|FJF-7|FJLJL-J|FJLJL7F7||L-7FJL7LJL7||F7||L-7FJ|LJL7FJLJFJ|||LJLJFJFJL7|LJLJL7FL--JF7|F-7LJF7||FJ|||F-JJFFJL|
|L7F7J--F7LLF-JL--JLJ|FJF------JL7|FJLJF7LF--JL--7|||LJ|F7||F-JF--J|||LJ|F7|L7|F--JL--7L-J|L-7F-JFJJFLJF----JF7F--JLJL7L7FJLJ|L-JLJ|JF|7|-FJ
L-.|..JFJJ|LL-------7|L-JF7F--7F-J|L---J|FJF-7F7FJFJL-7|||||L7FJF-7||L7FJ|LJ7LJL7F7F--JF--JF7||F7L-7F--JF----JLJF7F---J.LJF-7L--7F7L-7JFL-JJ
FL-JF7FJJ.|7F7F----7|||F7||L7FJL--JF-7F7||FJ.||LJFJF7FJ|||||FJL7||LJL7|L7L---7F-J||L--7L--7|||LJL7FJ|F-7|F-----7||L-7F7F77|FJF77|||F7L--7F-.
FLJ77|J7JF--J|L---7LJL-JLJL7||F-7F-JLLJLJLJ-FJL-7|FJ|L7|||||L7FJL--7FJ|FJF7F-J|F7||F-7L7F-J||L-7FJ|7LJ|LJL7F7F7LJ|F7LJLJL-JL-JL7||LJ|F--J7.L
JJ.L7JFFFL--7L----JF7F7F7F7LJ|L7|L---------7L7F-JLJJL7||||||FJ|F7F-JL7||FJ|L7FJ|||||FJFJ|F7||F7|L7|F----7FJ|LJ|F7LJ|F-7F-------JLJJFLJ|J|L-.
||F-|.JLF---JF----7|LJLJLJ|F7L-JL----7F7F-7|FJL-----7|||||||L7||LJF7FJ||L7L-JL7||||||FJFLJ||||||FJ|L---7|L-JF7LJL-7LJL|L--7LF7.F7.L|F|JL|7JF
LL|7|7..L-7F7|F---JL7F----J||F7F-7F-7|||L7LJL7F7F---JLJLJ||L7|||F-JLJFJ|FJF---J|||||||F---J|LJLJL-J|F--JL---J|F-7FJF-7L7F7L-JL7||7JLJJ.FJL7J
|.-L7-77LLLJLJL7F---JL7F-7FJLJLJ-LJFJLJL-J.F-J||L----7LF7LJJLJLJL--7FJ7||L|F7F7|LJ||||L-7F7|F---7F-7L-------7LJ-LJ-|FJJ|||F7F7LJ||.FJ.-JJ|.F
|-J-FJFJ77LF---J|F--7.LJ.LJF7F7F7F-JF7F---7L--J|F---7L-JL-7-F7F----J|F-JL7LJLJLJF-J|LJ|FJ||||F-7|L7L7F7F----JF7F---JL-7LJLJLJL--J7F-F-7LL7-J
|-L-JF7F77JL-7F7|L-7|F7|F7FJLJLJ|L--JLJF--JF--7|L-77|F7F-7|FJLJF-7F7|L7F7L-----7L--J-F7L-JLJLJFJL7L7|||L---7|||L7F7F--J7F7F7JF7F---7-JJ.FJLJ
L-7|LJ-7L-F--J|||F-J|||FJLJF---7|F--7F7|F-7L7FJL-7L7|||L7||L7F7L7|||L7LJL7F-7F7L-7F--JL-7F7F7FJF7L-JLJ|F--7|FJ|FJ||L-7F7|LJL7|LJF7FJJF|.F7FJ
FLF7-L.-..L---JLJL-7|||L-7FJFF-JLJF7LJLJ|FJFJL--7L7||||FJLJ-LJL7|||L-JF7FJ||LJ|F-JL7F--7||LJLJFJL----7|L-7LJ|FJL7|L--J|LJF-7LJF-JLJJ-777.FF7
LJJL7.-JF7F-7F7FF7FJLJL-7LJF7L-7F-JL--7FJL-JF---J7|||||L-7F7F--J|||-F-J|L7L--7|L7-LLJF7||L---7|F7F---JL-7L--JL--JL7FF7|F-J-|F7L---7.||JF-JJL
.LL-JLJ.|FL7|||FJLJF-7F7L--JL--J|F---7LJF7F7L----7||LJL--J|LJF--JLJFJF7L-JF7FJL7L7F--JLJL----JLJ|L-----7L-7F7F7F-7L-JLJL--7LJL---7|J-F-JJ7.L
77.||L-|7J||LJ|L7F7L7LJL7F-7F7F7|L-77L--JLJL7F---J|L7F7F7JL-7L7F---JFJ|F-7|||F-JFJ|F--7F-----7F7|F-----JF7||LJLJ.|F7F7F---JF7F7||LJJ-|.LF-7.
.7-LL7F|||FJF7L-J|L7|F--J|FJ|LJLJF-JF-----7|LJF--7L-J|||L-7||FJ|F7F7|FJ|.||||L-7L7||F-J|F----J|LJL-----7||LJF7F-7LJLJLJ|F--JLJL--7J|FJLFJ-J7
F.FLJ||LL7L-JL---J|LJL---JL7|F---JF7L7F---JF77|F-JF7F|||F-JFJL7|||||||FJFJ||L7FJFJLJL7FJL7F7F7|F------7LJL-7|||FJLF7F7F7|F-------J-F77-JJF-|
--7-F-77L|L|FLF----------7-LJL7F--JL-JL----JL7|L--J|FJLJ|F7L--J||LJ||||JL7|L7|L7L-7F-JL7LLJ||||L7F-7F7L-7F7LJLJL--JLJLJLJL7F7F-7F7F||F7FJ.-J
F7J7F-7-FL.|L.L7F7F7F--7FJF7F7LJF-7F7F7F---7FJ|F---JL7F-J|||F-7LJF-J|LJFL||.||FJF7||F7FJF--J|||FJL7LJ|F7LJL--7F7F-7F------J|||FJ||FJLJ|7JF-J
F|-7|-L.|JF7|F-J|||LJF-JL-JLJ|F7L7LJ|||L--7LJFJL7F7FFJL--JL-JFJF-JF7L7-|LLJ-||L7|LJLJ|L7|F-7|LJL7FJF-J|L-7-F7LJLJJ|L-------JLJL7|||F--J|F|7|
L77JF7.7777LFL-7|LJ-FJF--7F-7LJL7L-7LJ|F-7L--JF7LJL-JF7F7F---J|L--JL7|JL.|LJ||J|L--7FJFJLJ7||F-7||-L--JF7L-JL-7F-7|F7F7F7F-----J||||F7.L-LJJ
LL|7-|-|LF-7-.FJL7F-JFJF7LJLL--7|F-JF7LJ7L--7FJL7F---JLJLJJF7F7F77|L||-|FL--LJFJF7FJL-J7LF7LJL7LJ|F----JL-----J|FJLJLJLJ|L------JLJLJ|-F|||J
FJJ|-LL|-L7|FFL--J|F7|FJL-----7||L--JL7LF-7L|L-7|L---------JLJ|||F7.LJJ.F|-||JL7|||F7FF--JL7F7L-7|L--7F-----7F-JL-7F7F-7|F7F-7F-7F7F-J7--7|7
L-777...FFS|7F7JF7LJLJL------7LJL-----JFJFJFJF-J|F7F7F-7F7F7F7LJLJ|.JJFL-JFJ77||||LJL7L---7LJ|F7LJJF7LJF----J|F---J|LJFJLJLJJLJLLJ|L7J.J.--J
.LF7J..FFL7L7||FJL7-F----77F7L---------JFJFJFJFFJ|LJLJ-||LJ||L7F-7|-J.77FFFJJFFJ|L-7FJFF-7L-7LJL---JL-7|F7F--JL----JF7L7-F7JF----7L-J77JF7.F
J.|L7-|7|||FJ||L-7|FJF--7|FJL-----7F7F7FJ7L-JF7L7L7F7F7|L-7||FJL7LJ-|J|FFJ|L7LL-JJJLJ|FL7L-7|F-7F----7|LJLJF7F------JL-JFJL-JF---JJF7F7J||-|
|F7.J.|LLFJL-J|F7|LJFJF-JLJF-7F7F7LJ||LJFF7F-JL7L7LJLJ||F-JLJL-7L-7J|LJ-L-J7|.LLJ..|L-F-JF7LJ|FJ|F7.FJL----J|L-7F-7F----JF---JJF-7FJLJL777.J
FLL7F|7-FL--7FJ||L-7|FJF7F-J-LJLJL-7||F--JLJF-7L7L7F--JLJF7.||FJF-J-7.L7L--J.|J.F-FL.L|F-JL7FJL-J|L-JF-7F--7|F7LJFJ|F----JF77F7|FJL7F--J7L7J
7J|.F-|.FF--JL-JL--J||FJLJF7F7F7F7FJLJ|F7F-7L7|FJFJL----7|L7.FL7L7.|L..JJ777L7.LF7-L7-LJF7-LJF7FFJF--J7LJF-J||L7FJFJL----7|L-J||L7FJL----7|.
|F7.L7L-FJF7F-7F7F--JLJF--JLJLJLJ|L---J||L7L-JLJ-L7F----J|FJ7.FJFJF|-77|F7L-7|-|||LF|FLFJL---JL7L-JF7F7F7L7FLJLLJ.|F-----J|F--J|FJL7F7F--JJ7
|LJF.|L-L-JLJ7LJLJF7F-7L7F------7||F---JL-JF--7|F7LJF7F7L|L7-.L-JLFL7|J-LJ7.F7-FJ|7F7F-JF---7F7L---JLJ||L-JF7F7F7FJL---7F7||F--JL-7||LJJ|7LJ
|LL7.J||.FF-----77||L7|FJ|F-----JL-JF----77L-7L7||-FJLJL7|FJFF7|-L7-|JL-7LF7J.L|FJFJ|L-7|F--J|L7F7F--7|L---JLJLJLJF----J|||LJF-7F7LJL7LF|7.|
-77LF-F77LL----7L-JL-J|L7|L7F--7F7F-JF---JF-7L7||L7|F---J||-F7-|.7L-.77L|-J|F-J|L-JFJF7LJL---J7LJLJF7|L---------7FJF--7||||F-JJLJL-7FJ--FJ-L
|JL-7FL-|LF----JF----7|-||FJ|F-J|||F7|F77FJFJFJ||FJ||F7F-JL-J|JL-|7-|-L.|LFLJ.F|F-7L-J|-F7F7F7F7F7FJLJF7F7F7F--7LJFJF7L7|LJL----7F7||JJ-L--J
-7|LL7J.LFL-7F7FJF---JL7LJL-JL--JLJ|LJ|L7L7L7L7||L-JLJLJF7F-7|L7JLJ.LLJ-LJ.-7F-LJLL7F7L7|LJLJLJLJLJF--JLJLJ||F-JF-JFJL7LJF7F7F--J||LJJ....L-
L77.||FJ-J.LLJLJ.L----7L----7F7.F7FJF-JFJ7|FJFJLJF------JLJL||FJJFFJ.||J|FL-F-7-L--LJL7LJF-------7FJF7F7FF-J||F7|F-J.FJF7|LJLJLF7|L7J7F-.F7|
LL--JF-7|JFLF------7F7L----7LJL-J|L-JF7L--JL-JF-7L-7F7F7F--7LJJ--L-7F7-7F|JF|.FF7--JF-JF7|F----7FJ|FJLJL7L--JLJLJL--7L-J|L--7F7|LJFJJL7|.|L7
L|.|L|L|J-J7L-7F7F7LJL----7L----7L---JL---7F7FJFJF7LJLJLJF7L7JJ-LJ7L7.L|7||F-L7.L7JFL-7|LJL7F-7LJFJL7F-7L-7F7F7F7F--JF-7L7F7LJ||F7L-7.F7.LJ.
.|F77L-J77|LF-J|LJL7F7F-7FJF7F-7L----7F--7|||L7L7|L7F-7F-JL-J.|-JJFJLFJF7J-|FJ77L|FF7FJ|F7J|L7L7FJF-J|FJF7LJ|||||L--7|FJFLJ|F7|||L-7|-7|7.|.
LFLLLJJFF-LJL7FJF77LJLJF||FJLJFJF---7LJF-JLJL-J-LJJ|L7LJF---7F7|.-JJF|-||7-LL|LL-F-JLJFJ||FJFJFLJFL7FJL-JL7|LJ||L7F7LJL77F-J|LJ|L-7LJ.LL|-|7
.|L||-F|J|J-LLJF||F-----J|L--7L7L--7L-7L7JF7LF7F7F7L-JLFJF-7||L7-|J7FF7|L777.|7||L-7F-JFJ|L-J-F--7|LJ7F---JF-7||JLJ|F-7L7L-7L--JF-JF|F7.F..F
|LFLJF-777JLLF-7||L---7F7L---JFJF-7L-7L7L7||FJ||LJL7F--JFJ-LJL7L-7|FF||L7|F--7J-FF-JL7|L7|F7F7L-7L7F-7L----JFJLJF7LLJ-L-JF7L7F7FJ-LL-7..|-FF
|-J.F|FJF7|.LL7|||F7F7LJL--7F7L7L7|F7L7L-J|||FJ|F-7LJF-7L-7F7FJF-J7-FJ|FJ||F-JJ.FL-7FJF7|LJLJL-7L7|L7L7F---7L---JL-------JL7|||L-77L-..-J-FL
-.L7FJL-JL7-|FJLJLJLJ|FF7F7LJL7L-JLJL-J.F7|LJL7|L7L--JL|F7LJLJFJF7F7L7||FJ||7FL7.LLLJFJLJF--7F7L-JL7L7LJF7-L7F---7F7F---7F-JLJ|F7|7F|F7JJ.J|
L7-FL7F7F7L7FJF-7F--7|FJLJL7F7L---------JLJF-7LJFJF7-F7LJL----J||||L7||||FJ|F7J|||FF7L---JF7LJL-7F7L-JF-JL7FJL-7FLJ|L--7|L7F77LJLJF-7-J-F-.L
FF-LFJ|LJL7LJFJFJ|F-JLJF--7LJL---------7F7FJL|F7L-JL-JL-----7F7FJ|L7||||LJFJ|L-7J-FJL---7FJL7F-7|||F--JF7FJL7F7|F7|L-7FJL-J|L7F7|FJFJJL77J.J
||LFL-J.F-JF7L7L7|L7F7FJF-JF7F7F-----7J|||L7FJ||F-7F7F7F----J||L7|L||||L7FJ7|F7|F7L----7|L7FJL7LJ|LJF--JLJF7LJ|LJL--7|L7|F-JFJ|L7|FJJ--|F-|.
.LFJ-|LFJF7|L7|-LJFJ|||FJF7|||||F---7L7LJ|FJL7|||J||LJLJLF7F7||FJL-J|||FJL7FJ|LJ||7LF7J|L-JL-7L--JF7L----7|L-7L-7F7FJL7L7|F-JFJFJ||F77F|L7L7
7.-JLL|L-J|L7||F--JFJLJL-JLJLJLJL--7|FJF7LJFFJ||L7|L-7F--JLJLJ||F-7FJ||L-7|L7L7FJL7FJL7L---7FJFF7.|L-7F-7LJF-JF7LJ||F7L7LJL--JFJ.|LJL---77-J
LJJF7-|FL.|FJ||L---JJF7FF7F-7F-7F--JLJFJL--7|FJ|FJ|F7|L7F7F7F7|LJFJL7||F7|L-JFJ|F7|L-7L-7F7||F-JL7L-7LJFJF7L7F|L-7|LJL7|F7F---J-FJF-7F7FJJ.|
.LLJJ.L7F-LJ7LJF7F---JL7||L7|L7|L-----JF7F-JLJ-LJ7LJLJFJ|||LJLJF7L-7||LJ|L7F-JF||LJF-JF7LJLJ|L7F7L-7L-7L-JL7L-JF-JL-7FJLJ|L---7FJFJLLJ||J.F7
F.FJ.J---J||7F-J|L----7||L-JL-JL--7F7F-J|L------7F7F7-L-J|L-7F-JL--JLJF7|FJL-7FJL7-L--JL---7L7LJL-7|F7L--7FJF--JJF-7LJF-7L----JL7L-7L|LJLLLF
|-7.LJ.|.LL|FL-7|F7F--JLJF7F7F-7F7LJLJF7L----7F7||LJ|F7F-JF-JL------7FJLJL-7FJL7FJF7F7F7JF7L7L7F--JLJL---JL-JF7F7L7L7J|FJ.F7F---JF7L77LJ|.F|
||F-77-777FJJF-JLJLJF7F-7|||LJFJ||F7F7||F7F77||LJ|F-J||L-7L-7F--7-F7|L7F7F7||7|||F|LJLJ|FJ|||FJL----------7F7|LJL-JFJFJL7FJ||F-7FJL-J7J7L7LJ
LFL7||.|-F7JJL------J||FJ|LJLFJFJLJLJLJLJLJL7LJF7||7FJ|F7|F-J|F7L-J|L7LJ||||L7FJL7|F7F-JL7|FJ|7F7F--------J|||F----J|L7FJL7||L7|L--7.J.|FJ.|
F-7J-J-FJFJFF7F7F77F7LJL-JF-7L-JF----------7L7FJLJL7L7||LJL7FLJL--7L7|F-J|||FJ|F-JLJ|L-77||L7|FJ|L--------7|LJL---7F7FJL77|||FJL---J7JFLJF7-
LLJJJ|..FF--JLJLJL-JL-----JFJF7-L---------7|FJL---7L-J|L--7L-7LF7LL7||L-7|||L7||F7F-JF-JFJL-J||FJLF7F7F---J|F-----J|LJF-JFJLJL-7F7JF7F7.L7J|
F|F|F--7-L--7F-7F---7F7F--7L-JL7F----7F7F-J||F-7F7|F-7L-7.L-7|FJL--J||F-J||L7LJLJ|L-7L-7L7F-7|||F7||||L7F77|L-7F7F7|F-JF-JF-7F7LJ|FJ|||77|.|
-7|FL|7L|L7FJL7||F--J|LJF7L---7|L---7||LJF7LJL7LJ||L7L--JF7FJ|L-7F-7LJL--JL7L-7F-JF7L7FJ-LJFJ||LJ||||L7LJL-JF7LJLJLJL--JF-J.||L--J|FJ||LF|-|
LFLJJF--FJFL7FJLJL---JF-JL----J|F7F-JLJF7|L--7|F-J|FJF7F7||L7|F7LJJL--7F-7FJF-J|F-J|FJ|7FF7L7LJF-J||L7L---7FJ|F--7F--7F7L--7LJF---JL-JL7-|F|
|L|77J7.|LF7LJF7F----7L--7F7F-7|||L7F-7|||F--J|L7FJL7|||LJ|.||||F7F--7|L7LJ7L-7LJF-JL7L7FJL7L7FJ-FJ|FJF--7LJFJL-7|L-7||L7F7L--JF7F7F7F-JJJL7
|FJL-.|7-FJL7L||L---7L7F7||LJFJLJL-J|FJ||||F-7L7|L7FJ||L7FJFJLJLJ|L7FJ|FJ.F---JF-JF7|L7|L-7|.||F7|FJL-JF7L-7L---JL--JLJFJ|L7F--JLJ||LJ.L7JF-
F|FJJ-L7FL-7L-JL---7L7LJ|LJF7L7F----JL-JLJ||FJFJL7|L7||FJ|LL-7F--JFJL-J|F7L---7|F7||F7||F-JL7|||||L-7F7|L--JF--7F7F7F7-L7L7|L----7||JJ-|--|J
LL--J.|J-|-L7F--7F7L-JF7L7FJL-J|FF7F7F7F--J|L7L-7LJFJ|||FJF7FJL7F7L-7F-J|L7F77||||||||||L7F7||LJ||F-J||L--7.|F-J|||||L-7|FJL-----JLJ..FJ.|.F
F7|LLL|7FL7J||F-J|L-7FJL7LJF7F7|FJLJLJLJJF7|FJF-JF-JF||||F||L7FJ||F7|L-7|FJ||FJLJLJ|||||FLJ||L7FJ|L7FJL---JFJL--JLJLJF7||L-7F77FF7J.7F||.FFJ
L--F-7LL-.|JLJ|F-JF-J|F7L-7|LJ||L-----7F7|||L7L-7|F-7||||FJL-J|FJ||LJF-J|L7||L-7F--J|||L7F7|L7|L-JFJL------JF7F------JLJ|F7LJL--JL777-J-F-F.
||.|L-.LL-|-F-J|F-JF7||L--J|F-J|F-----J||||L7|F7||L7||||||F---JL7|L7FJF7L7|||F7||F77|||FJ||L7|L7F-JF7F7F7F-7||L--------7LJL7F----7L7--|-L.|J
FJ7JL|F-|7|LL-7|L--JLJL7F-7|L--JL--7F7FJLJ|FJ||||L7|||||||L--7F7|L7||FJL7|||||||LJ|FJ|||FJL7||FJ|F7|LJ|||L7LJ|F--7F7F7FJ.F7|L---7|FJJFLJ|FLJ
7L7.FF7-FF7LLL||F--7F--J|FJ|F-7F---J||L--7LJ-LJLJFJ||||||L7F-J|LJF||LJF-J|||LJ|L-7|L7||||F-J|||FJ|||F-J|L7L7||L7LLJLJLJF-J|L--7FJ|L7|FJFFLJ7
||.F--|-7.-J|LLJL-7|L---JL7|L7|L7F-7||F-7L7F-----JFJLJLJ|FJL-7L--7|L-7|F7|||F-JF7||FJ||LJL7FJ|||FJ||L-7L7L7L7L7L---7-F-JF-JF-7|L7L-J-JL||||7
F|-7.LL7LFLFFJJLF-JL7|F7F7||FJL7LJFJ|||7L-JL---7F7L-7F--J||F7|F--JL7FJ||||||L7FJ||||FJL-7|||FJ||L7|L7FJFJ.|FJJL-7F7L7L7FJF-JFJ|FJJF|-F.LF|J7
.J-7FL|7.7-LJF-JL--7L7||||LJ|F7|.FJFJ|L----7.F-J|L7FJ|F-7|FJLJL--7FJL7||||||FJL7||LJ|F7FJFJ||FJL-J|FJL-JF-JL---7||L7L-JL-JF7||LJJF7F7-77||.|
JF777.FJ.J-|77J-||FL7LJLJ|F7LJ||FJFJFJF7F-7L7L7FJFJ|JLJFJ||F7F7F-J|F-J||LJ||L7FJ|L7FJ||L7|FJ|L7F7L|L7F--JF-7F7FJ|L7|F-7F--JLJF-7FJLJL-77LF77
F7|.F7LF-J..7-F--7F7L---7LJL7FJ|L7|-L7|LJ7L7|FJ|FJFJF7FJFJ|||||L-7|L7LLJ|FJ|FJ|FJFJL7||FJLJJL7||L-JFJ|F-7|.|||L7L7|LJFJL---7JL7||F--7FJ-7J||
L|L77L-F7L7.J|L-7LJL----JF-7|L7L-JL-7|L-7F7LJL7|L7|FJLJFJFJ||||F7|L7L---7|FJL7|L7|F7|||L---7-||L7F7|.LJFJL7LJ|FJ7|L-7L7F7F7L--JLJ|J-LJ.L|7||
L|.LF7..-F|7JL7.L-------7|FJL-JF7F7FJL-7LJL--7|L7LJL--7|FJFJ|||||L7|F---J|||FJL7|||LJ||F--7|FJ|FJ|||F--JF7L7FJL-7|F7|FJ||||F-7F-7L-7FJ77.L7J
-F-LLJ-FJLL7-7F.F------7LJL7F7FJ|||L--7|F-7F-JL-JF----J|L7|FJ|LJ|FJ|L--7FJL7|F7|||L7FJ|L-7||L7|L7|LJL7F7|L-JL7F-JLJLJL7||||L7||FJF-J-J-FJ-JL
|.L.|7.L7-7|F|7-L-----7L---J|||FJ||F7FJ|L7|L--7JFJF-7F7L7||L7L7||L7|F7FJL7FJ||LJLJFJL7|F7||L7||FJ|F--J||L---7|L---7F7FJ||||FJ||L7L7.JJL|7L7.
JJ.|F|7.F--7FJF7F7F7F7L----7||||FJ||LJFL7||F--JFJFJFJ|L7|||FJFJFJFJ|||L7FJ|L||7F--JF-J||LJ|FJ||L7|L-7FJ|F---JL7F-7LJ||FJ||LJ-LJL|FJ7.-7LLF|J
|F7J7||-J-LLJF|LJLJLJL-----J|||||F|L--7FJ||L--7L-JL|FJFJ|||L-JFJFJFJ|L-JL7L7||FJF7FJF7||F-J|FJL7LJF-J|FJL7F--7|L7|F-JLJ-|||JJ.F-JL7J77J.|LL7
-FLJLL|||7.LF-L-7F---------7||||L7|F--JL-JL-7FJF---JL7L-J||F--JFJ-L7L---7L7|||L7|||J|LJ|L-7||F7|F7|F7|L-7LJF7||FJ|L----7|L7J.FJF--JF77|-L7||
L|J.7-L7J--.L-LLLJ7F7F-----J|||L-J|L------7FJ|FJF7F--JF--J||F7FJF7F|F7F7L7||LJL||LJFJF7L7J||LJ|||LJ|||F-JF-JLJ|L7|F7F7FJ|FJ-7L-J|LJJLF-.|LFJ
LJ.L|-LJL7|.|||JJF-JLJF7F--7||L-7FJF7F7F--JL-JL7||L-7JL-7FJ|||L-J|FJ|||L7|LJF--JL7FJFJ|FJFJ|F7LJL-7|LJL-7L-7F-JFJLJLJ|L7||-L|7|.L.|.|L--L-7.
.F-7L-JJ.||.F|..FJF7F7|||F-J||F7||FJ|||L---7-F7LJ|F7L-7FJL7|||F--JL7|||FJ|F7|F-7FJL7L7||FJFJ||F7F-JL-7LFJF-JL-7L--7LFJFJ||77|F|7F7-7F7L7L--.
-7LJ||||7LJ7LL7FL7|LJ|||||F7||||||L7|||F-7FJFJL--J||F-JL7FJLJ||F--7LJ||L7LJ|||FJ|F-JFJ||L7|FJLJLJF-7FJFJFJJF-7L7F7L7|FJLLJ--L-|-7J7F|7|J7|FJ
FJ.LFF7F|7JFJ.F--LJF-J|||||||LJ|||FJ||||-|L7|F-7F7|||JF-JL-7FJLJF-JF7LJLL-7|LJ|FJ|F-J.||FJ|L7F7F-JFJ|FL7|F7|FJ-|||FJ||-|J-|||FJ-LF-7.FJFL77.
F-7-LJ-F----..F-7JFL-7||||||L7FLJ||FJ||L7L-J||FJ|||||FJF--7||F-7L--JL-7F--JL-7|L7||L|-|||FJ-LJ||F-JFJF-JLJLJL-7||LJ7|L-7J7.FJL-F-JFLL|7FLL-7
F||.J.F|||7|.-77F-L.|||LJ||L7L7F7LJL7|L7L-7FJ|L-J|||||FJF-J|||FJF-7F7FJL--7F7|L-JLJ|.FLJ||F---J||F7L7L7F7F7F7FJ|L7-LL7FJ-F|L-L-J--J7LLJ77L-F
FL-77-F|7-L|7F77.L|-FLJ-FJL7L7LJL--7LJ.L7FJ|FJF--J||||L7L-7|||L7||||LJJ-L-LJLJJJ|LJ7J-JJLJL---7||||FJL||||||LJF|FJ..LLJ7-|.F7-L||7.J7J.LL|-J
|JF--7FJ.|LL7F||7.|J|LL-L7FJFJF-7F7|.FJL||7|L7L---JLJL7L-7||||FJL7|L--7FFL7L7|J.F7J.LFJ|7.LF--J||||L-7LJ|||L7J-||77F|JL7.FL777.--7J-JJFF|.|7
|F7JL|J.FJ-.F7LJF7JLLJ|F-JL7L7L7LJ|L-7J7||FJFJF-------JF-J|||||F7||F-7L77-LJLJL7|--7-F7|J-7L-7FJ|||F-JLL|||FJJFLJ--JL7JJF-7LJ-7JFLJL|-LJJ--J
-|JFFJ7-LLJ-J-.LL-..LF7L7F7|7L7L7JL--JLF|||FJFJF7F7F7F7L-7|||||||||L7L-J-7|..-7.|-FFJL7..FJ-L||FJ||L-77JLJ||LJ7-JJ||7|7|J.LJLJFJ..|F77-FJL||
F|F.|-F.FJJ.|.F7.|-F.F7J||LJJ.L7L-7LJJFLLJLJFJFJ|||||||F7|LJLJLJLJL-JJJF|7J|J-|-|.LJ.FL-J77FFJ||FJ|F7||7L-LJ7L7-J.JJ-L|-.F.7LF-7F-FL||.LJ.|7
L-|-77L7F-7-J-J|.|J.-|L-LJL|7LFJF-J.L-|-JJ-LL-JL||||||||||FJ|7|-J-LJ.|FJJF-L7LF7J7LJF-7FL|7F|FJ||JLJLJ-L.||FFF7L7-J7JL7F|.FJ-L-J7JF7LL77|-F7
J.F..F-F|LJ77|FJF7..LL7|.JFLF7L7L77F7.|FJ|-LL|JJLJ||||||||JLL-.F||LL-JJF-7|LL.LJFF7F7.|.-F--JL7LJ-JF|||L-7J|J|J-JLF|-|L-L-.JL|J7JL--7J|FL-F7
.FF7LL7.|-|-7FJ.|JF|.J|J-|F-LJ.L7L7L-.7JF77.||-F|FLJLJ||LJ.F|--J77-LJ-FJ---.|-L7-77FJ-77|L----J-JJ.|7.F|-|.L7L7FJ.F.F7-LLJ7.F--J-F|F|.|7|LJF
.FL|J.|F77J.F---F7-|-7L7|FF.7.-.L-J-L.|LF|L7LJ-F|FJ|.L||LLJF777L77F7J7FJ-LLF77.J-LJJ.L|-F|||JFJFL7.777.L7F.F|J---7LLF7|FJ.FJ7L7JFL7-L-LLJJLL
-77JLFF7|J7.7||F|J-J.FLL7.77.7.|JLJ|.F|-FF---JJJLJ.F7FLJ-.|FJ---77-L7-J.7||LJ7F-F-.F-.|L-7-JJF.7-FF||-|FJJ-LJ|JF77JJ|F7.|77F--J-L-J|.F|7LJF7
FLF--7L-J7|-LL|-|J.J7--JF|77F|.FJLFJ.L|-FJ7.|F7-|7.7.LJJFF.JJJFL.|||FJ|-L--7JF--|FFF|---7J|LF-JJF--7--JL.L7.L|.|L|JLF7JF7-FJ|.F.F|.J.-7J|F-7
F7|-FJ|L|-J7.F--F7|FJF|7L|||F77J-FFFL7.F|J|F-FJ|.F.--------|.|F.---7L-LF|-L7L7JL|L7.|.|7F-J.JJ.--|7|||.JF-F7|L|.|.|.FL-7L|L|7..|7-|.|LJ||7FL
LJJJ||L-J7LJ7F|-LLJ|7L--7L-J-.|F--FJ.|7LL.|L--7-7L7|L||-7JJL-JJLLJFF7J.LL77..J.F|.|7LF---J.FL.JLF|LJJ-F-F..F--|.77F7-JJL77.||.|F7.LFL77|J7FJ
LJJ..LL-JJ-L---7J-F77LL.LFJJLL-J-7JJ.L-LL7-L7.LLJL|JJF7-J-JLLJLLJ-7LJJ..LLJ-LJJ.LFL-FJJL|.FLLF-FL|LL.J.FL|-JJ.JJL--JLL--JJ.|J-L|J--J.L7..JJJ
```

This yields:
```
......................................................................F7....................................................................
......................................................................||...F7....................F7.F7F7....................................
.....................................................................FJL7..||........F7..........|L7||||....................................
...................................F7F7...........................F7.L7FJF7||F7......||F7........L7||LJ|....................................
...................................|LJ|F7F7................F7.F7.FJ|F7||FJ||LJL-7...FJ|||.......F7|||F7|....................................
...................................L-7LJLJL7......F7.F7...FJL-JL7L7||||||FJL-7F-J...L7LJL7F--7.FJLJLJ|LJ....................................
.....................................L7F---J......||FJ|F-7|F---7L-J||||LJL--7|L--7...L7F-J|F-J.L--7F-JF7....................................
.................................F-7F7|L--7......FJ||FJL7|LJF--JF-7LJ||F----J|F--JF7F-J|.FJ|...F7FJ|..||....................................
.................................L7LJLJF--JF-7...L7|||F7||F7L7F-J.L-7LJL-7.F7||F--J|L-7|FJFJ.F-J||FJ..|L7...................................
..................................L---7L-7.|FJ...FJLJLJLJLJ|.LJF7.F-JF---JFJLJLJF7FJF7||L7|F7L-7||L7F7L7|...................................
...................................F7FJF-JFJL7...L-7F7F7F-7L7F-JL-JF7L7F7.L7F---JLJFJ|||FJLJL7FJLJFJ|L-JL7..................................
..................................FJ|L7L7FJF-J...F-J|LJ|L7L-JL---7FJL7LJ|F7|L-7F7F7L7||LJF---JL7F-J.L7F7FJ..................................
..................................|FJ.|FJL7|.F7..L--JF-JFJ.F7.F--J|F7L-7LJLJF-J|LJ|FJ||F-J.F7F7|L-7F-J|LJF7.................................
...............................F7FJL7FJL7FJL7|L-7F7F7L7FJF7|L7L---J||F-JF---JF-JF7||FJ|L--7|||||F-JL-7L--JL7................................
............................F-7||L7FJL-7||F7||F-J||||FJL-J||FJF----J|L-7|F7F7L--J||||FJF--J|LJLJL7F7FJF----J.....F7.........................
............................L7LJL-J|F7FJLJ||LJL7FJ||LJF---J||.L7F7F7L7FJ||LJL--7.||||L7L7F7L-7F-7|||L7L----7F7...||.........................
.............................L----7|||L-7FJL---JL7|L-7|F7..||F7LJLJL7|L7||F----JFJLJ|FJFJ|L-7||.||||FJF----J||F7FJL7.F7.....................
.............................F---7|LJ|F7|L-7F7F7FJL7FJ||L7FJLJ|F7F7FJL-JLJL7F7F7L--7|L7|FJF7LJL7LJ||L7|.F7F7|||LJF-J.|L7....................
.............................L7F7LJF-J|||F-J||||L7FJL7||FJ|F--J||||L--7F7F7LJ|||F--JL-J||FJL-7FJF-J|.|L-J||LJ|L-7L---JFJ....................
.........................F7..FJ|L-7L7FJ||L-7||||.||F-J||L7|L7F7|||L7F-J|LJ|F-J||L-----7LJL-7FJ|FJF7L7|F--J|F-JF-JF7F7FJ..F7.................
.....................F--7||F7L-JF7|FJL7LJF-J||||FJ|L-7||FJL7||||LJFJL--JF7||F7|L7.F7F-JF---JL-JL-JL7|||F7FJL7.L7FJLJLJF-7||.F-7.............
..................F7.L-7|||||.F-JLJL--JF7|F7||||L7L7FJLJL7FJLJ|L-7|F7F-7|LJ||LJFJFJLJF7|.F7F7F7.F7FJLJ||LJF-JF7||F7F-7|FJ|L7L7|.............
..................||F7.|LJLJL7L--7F7F-7|LJ|||LJL7|FJL7F--JL7F7|F-JLJ|L7|L-7LJF7L7L---J|L-JLJ||L7||L-7FJ|F-JF7|||LJLJFJ||FJFJFJL7............
.................FJLJL-JF7F-7L7F7LJLJFJ|F-J||F--J|L7FJL7.F7||LJL--7FJFJL--JF-JL-J.F---JF7F--J|FJ|L7FJL-JL-7|||LJF--7L7||L7L7|F7|............
................FJF-----JLJ.|FJ||F7F7L7LJF7LJ|F--JFJL7FJFJ||L7.F7FJL7L-7F-7L7F---7L----J|L7F7|L7L7|L7F7F--J|||F-JF7L-J||.L7|||LJ............
................L-J.F7F--7F-J|FJLJ|||.L7FJ|F-JL--7|F7||.|FJL7L7||L-7L--JL7L7||F--JF7F---JFJ||L7|FJL-J|LJF7.|LJ|F-JL--7||F7|LJL--7...........
................F7.FJ|L-7|L-7|L--7LJ|F7||.|L-7F-7|||LJL7||F7L7||L7.L---7FJ.|LJL-7.||L---7L-J|FJ|L---7L7.|L7L-7LJF----J|||||F----J...........
...............FJL7L7|F7||F-JL7F7L-7||LJL7L--J|FJ||L-7FJ||||.|||FJF7F7.LJF7|F---JFJ|F7F7|F--J|FJF--7L7L-JFJF7|F-J.F--7|LJ|||F7F7............
.............F-JF7L7||||||L-7FJ|L7FJ||F--JF7F7||FJL7FJL-JLJL7|LJL7|LJ|F7FJLJL---7|FJ|LJ|||.F7|L7|F-JFJF--JFJLJ|.F7|F7|L-7LJLJLJ|F7..........
.............|F-JL7|||||||F7|L-JFJL7|||F-7|||LJ||F-JL-7F----JL-7FJL-7||LJF-7F7F7||L7L-7LJL7|||FJ|L-7L7L7F7|F--JFJ|||LJF7|F7F--7LJL7.........
.............LJF--JLJ||||||LJF-7L--J||||FJ||L-7|||F7.FJ|F7F7.F-JL7.FJ||F-J.||LJLJ|FJF7L7F-J|LJL7|F7|FJFJ||||.F-JFJ|L-7||LJ||F7L---J.........
...............L----7|||||L-7|.L---7||||L7|L-7|||LJL7L7||||L7L-7FJFJFJLJF7FJL---7||FJ|FJL-7L-7FJLJ||L7|FJLJL7|F-J.|F-J|L7.|LJL-7F--7........
..............F-----JLJLJ|F7LJF7F7.||||L7||F-J||L--7L-JLJ||FJF-JL-JFJF7FJLJF----J||L7|L7F-JF7|L-7.||FJLJF---J||.F-JL-7L7|FJF7F-J|F-J........
............F7L--7F7F-7F7LJL--JLJ|FJLJL7|LJL7FJL-7.L-7F--J|L7L----7L7||L--7L-7F7FJL7||FJL-7|||F-JFJ|L-7FJF7F7|L-JF---JFJLJFJ|L--JL7.........
...........FJL--7LJLJ.LJL7F7F---7|L---7|L-7FJ|F--JF7.||.F7|FJF7.F7L7|||F7.|F-J||L7FJ||L7F-J||||F7L7|F7||FJLJLJF--JF7F7|F7FJ.L7F-7FJ.........
...........L---7L7F-7F---J|LJF--JL7F-7|L7FJ|FJ|F7FJL7|L7|||L7|L7|L7|LJ|||FJ|F7||FJL7||FJ|F7|||||L7|LJ||||F---7L7F-J|||||LJF7.||.LJF7F7......
...............L7LJFJL----JF7L7F-7LJ.|L7||FJ|FJ|||F-JL7||LJ.||.||FJ|F-J|||.LJ||||F-J|||.||||||||.|L7FJ||||F7.L7LJF-J|LJL7FJL7|L7F7|LJ|......
..............F7L-7|F7.F---JL-J|FJF-7|FJLJL-J|FJLJL7F7||L-7FJL7LJL7||F7||L-7FJ|LJL7FJLJFJ|||LJLJFJFJL7|LJLJL7.L--JF7|F-7LJF7||FJ|||F-J......
............F-JL--JLJ|FJF------JL7|FJLJF7.F--JL--7.||LJ|F7||F-JF--J|||LJ|F7|L7|F--JL--7L-J|L-7F-JFJ..LJF----JF7F--JLJL7L7FJLJ|L-JLJ|........
............L-------7|L-JF7F--7F-J|L---J|FJF-7F7FJFJL-7|||||L7FJF-7||L7FJ|LJ.LJL7F7F--JF--JF7||F7L-7F--JF----JLJF7F---J.LJF-7L--7F7L-7......
............F7F----7||.F7||L7FJL--JF-7F7||FJ.||LJFJF7FJ|||||FJL7|.LJL7|L7L---7F-J||L--7L--7|||LJL7FJ|F-7|F-----7||L-7F7F7.|FJF7.|||F7L--7...
.........F--J|L---7LJL-JLJL7||F-7F-J.LJLJLJ.FJL-7|FJ|L7|||||L7FJL--7FJ|FJF7F-J|F7||F-7L7F-J||L-7FJ|.LJ.LJL7F7F7LJ|F7LJLJL-JL-JL7||LJ|F--J...
.........L--7L----JF7F7F7F7LJ|L7|L---------7L7F-JLJ.L7||||||FJ|F7F-JL7||FJ|L7FJ|||||FJFJ|F7||F7|L7|F----7FJ|LJ|F7LJ|F-7F-------JLJ..LJ......
........F---JF----7|LJLJLJ|F7L-JL----7F7F-7|FJL-----7|||||||L7||LJF7FJ||L7L-JL7||||||FJ.LJ||||||FJ|L---7|L-JF7LJL-7LJ.|L--7.F7.F7...........
........L-7F7|F---JL7F----J||F7F-7F-7|||L7LJL7F7F---JLJLJ||L7|||F-JLJFJ|FJF---J|||||||F---J|LJLJL-J.F--JL---J|F-7FJF-7L7F7L-JL7||...........
..........LJLJL7F---JL7F-7FJLJLJ.LJFJLJL-J.F-J||L----7.F7LJ.LJLJL--7FJ.||.|F7F7|LJ||||L-7F7|F---7F-7L-------7LJ.LJ.|FJ.|||F7F7LJ|...........
...........F---J|F--7.LJ.LJF7F7F7F-JF7F---7L--J|F---7L-JL-7.F7F----J|F-JL7LJLJLJF-J|LJ.FJ||||F-7|L7L7F7F----JF7F---JL-7LJLJLJL--J...........
...........L-7F7|L-7|F7.F7FJLJLJ|L--JLJF--JF--7|L-7.|F7F-7|FJLJF-7F7|L7F7L-----7L--J.F7L-JLJLJFJL7L7|||L---7.||L7F7F--J.F7F7.F7F---7........
..........F--J|||F-J|||FJLJF---7|F--7F7|F-7L7FJL-7L7|||L7||L7F7L7|||L7LJL7F-7F7L-7F--JL-7F7F7FJF7L-JLJ|F--7|FJ|FJ||L-7F7|LJL7|LJF7FJ........
..........L---JLJL-7|||L-7FJ.F-JLJF7LJLJ|FJFJL--7L7||||FJLJ.LJL7|||L-JF7FJ|.LJ|F-JL7F--7||LJLJFJL----7|L-7LJ|FJL7|L--J|LJF-7LJF-JLJ.........
..........F-7F7.F7FJLJL-7LJF7L-7F-JL--7FJL-JF---J.|||||L-7F7F--J|||.F-J|L7L--7|L7..LJF7||L---7|F7F---JL-7L--JL--JL7.F7|F-J.|F7L---7.........
..........L7|||FJLJF-7F7L--JL--J|F---7LJF7F7L----7||LJL--J|LJF--JLJFJF7L-JF7FJL7L7F--JLJL----JLJ|L-----7L-7F7F7F-7L-JLJL--7LJL---7|.........
...........|LJ|L7F7L7LJL7F-7F7F7|L-7.L--JLJL7F---J|L7F7F7.L-7L7F---JFJ|F-7|||F-JFJ|F--7F-----7F7|F-----JF7||LJLJ.|F7F7F---JF7F7..LJ.........
..........FJF7L-J|L7|F--J|FJ|LJLJF-JF-----7.LJF--7L-J|||L-7.|FJ|F7F7|FJ|.||||L-7L7||F-J|F----J|LJL-----7||LJF7F-7LJLJLJ.F--JLJL--7..........
..........L-JL---J.LJL---JL7|F---JF7L7F---JF7.|F-JF7.|||F-JFJL7|||||||FJFJ||L7FJFJLJL7FJL7F7F7|F------7LJL-7|||FJ.F7F7F7|F-------J.F7.......
..............F----------7.LJL7F--JL-JL----JL7|L--J|FJLJ|F7L--J||LJ||||.L7|L7|L7L-7F-JL7.LJ||||L7F-7F7L-7F7LJLJL--JLJLJLJL7F7F-7F7.||F7.....
..............L7F7F7F--7FJF7F7LJF-7F7F7F---7FJ|F---JL7F-J||.F-7LJF-J|LJ..||.||FJF7||F7FJF--J|||FJL7LJ|F7LJL--7F7F-7F------J|||FJ||FJLJ|.....
.............F-J|||LJF-JL-JLJ|F7L7LJ|||L--7LJFJL7F7.FJL--JL-JFJF-JF7L7...LJ.||L7|LJLJ|L7|F-7|LJL7FJF-J|L-7.F7LJLJ.|L-------JLJL7|||F--J.....
.............L-7|LJ.FJF--7F-7LJL7L-7LJ|F-7L--JF7LJL-JF7F7F---J.L--JL7|......||.|L--7FJFJLJ.||F-7||.L--JF7L-JL-7F-7|F7F7F7F-----J||||F7......
.........F-7..FJL7F-JFJF7LJ.L--7|F-JF7LJ.L--7FJL7F---JLJLJ.F7F7F7...||......LJFJF7FJL-J..F7LJL7LJ|F----JL-----J|FJLJLJLJ|L------JLJLJ|......
.........L7|..L--J|F7|FJL-----7||L--JL7.F-7.|L-7|L---------JLJ|||F7.LJ........L7|||F7.F--JL7F7L-7|L--7F-----7F-JL-7F7F-7|F7F-7F-7F7F-J......
.........FJ|.F7.F7LJLJL------7LJL-----JFJFJFJF-J|F7F7F-7F7F7F7LJLJ|............|||LJL7L---7LJ|F7LJ.F7LJF----J|F---J|LJFJLJLJ.LJ.LJ|L7.......
.........L7L7||FJL7.F----7.F7L---------JFJFJFJ.FJ|LJLJ.||LJ||L7F-7|...........FJ|L-7FJ.F-7L-7LJL---JL-7|F7F--JL----JF7L7.F7.F----7L-J.......
..........|FJ||L-7|FJF--7|FJL-----7F7F7FJ.L-JF7L7L7F7F7|L-7||FJL7LJ...........L-J..LJ..L7L-7|F-7F----7|LJLJF7F------JL-JFJL-JF---J.F7F7.....
.........FJL-J|F7|LJFJF-JLJF-7F7F7LJ||LJ.F7F-JL7L7LJLJ||F-JLJL-7L-7...................F-JF7LJ|FJ|F7.FJL----J|L-7F-7F----JF---J.F-7FJLJL7....
.........L--7FJ||L-7|FJF7F-J.LJLJL-7||F--JLJF-7L7L7F--JLJF7...FJF-J...................|F-JL7FJL-J|L-JF-7F--7|F7LJFJ|F----JF7.F7|FJL7F--J....
.........F--JL-JL--J||FJLJF7F7F7F7FJLJ|F7F-7L7|FJFJL----7|L7..L7L7..............F7....LJF7.LJF7.FJF--J.LJF-J||L7FJFJL----7|L-J||L7FJL----7..
........FJF7F-7F7F--JLJF--JLJLJLJ|L---J||L7L-JLJ.L7F----J|FJ..FJFJ..............||.....FJL---JL7L-JF7F7F7L7.LJ.LJ.|F-----J|F--J|FJL7F7F--J..
........L-JLJ.LJLJF7F-7L7F------7|.F---JL-JF--7.F7LJF7F7.|L7..L-J..............FJ|.F7F-JF---7F7L---JLJ||L-JF7F7F7FJL---7F7||F--JL-7||LJ.....
..........F-----7.||L7|FJ|F-----JL-JF----7.L-7L7||.FJLJL7|FJ...................|FJFJ|L-7|F--J|L7F7F--7|L---JLJLJLJF----J|||LJF-7F7LJL7......
..........L----7L-JL-J|L7|L7F--7F7F-JF---JF-7L7||L7|F---J||.F7.................|L-JFJF7LJL---J.LJLJF7|L---------7FJF--7.|||F-J.LJL-7FJ......
..........F----JF----7|.||FJ|F-J|||F7|F7.FJFJFJ||FJ||F7F-JL-J|.................|F-7L-J|.F7F7F7F7F7FJLJF7F7F7F--7LJFJF7L7|LJL----7F7||.......
..........L-7F7FJF---JL7LJL-JL--JLJ|LJ|L7L7L7L7||L-JLJLJF7F-7|.................LJ.L7F7L7|LJLJLJLJLJF--JLJLJ||F-JF-JFJL7LJF7F7F--J||LJ.......
............LJLJ.L----7L----7F7.F7FJF-JFJ.|FJFJLJF------JLJ.||.....................LJL7LJF-------7FJF7F7.F-J||F7|F-J.FJF7|LJLJ.F7|L7........
............F------7F7L----7LJL-J|L-JF7L--JL-JF-7L-7F7F7F--7LJ......................F-JF7|F----7FJ|FJLJL7L--JLJLJL--7L-J|L--7F7|LJFJ........
............L-7F7F7LJL----7L----7L---JL---7F7FJFJF7LJLJLJF7L7.......................L-7|LJL7F-7LJFJL7F-7L-7F7F7F7F--JF-7L7F7LJ||F7L-7.......
............F-J|LJL7F7F-7FJF7F-7L----7F--7|||L7L7|L7F-7F-JL-J..........F7..........F7FJ|F7.|L7L7FJF-J|FJF7LJ|||||L--7|FJ.LJ|F7|||L-7|.......
............L7FJF7.LJLJ.||FJLJFJF---7LJF-JLJL-J.LJ.|L7LJF---7F7........||........F-JLJFJ||FJFJ.LJ.L7FJL-JL7.LJ||L7F7LJL7.F-J|LJ|L-7LJ.......
.............LJ.||F-----J|L--7L7L--7L-7L7.F7.F7F7F7L-J.FJF-7||L7.....F7|L7.......L-7F-JFJ|L-J.F--7.LJ.F---JF-7||.LJ|F-7L7L-7L--JF-J.........
.....F-7.....F-7||L---7F7L---JFJF-7L-7L7L7||FJ||LJL7F--JFJ.LJL7L-7...||L7|F--7...F-JL7.L7|F7F7L-7L7F-7L----JFJLJF7.LJ.L-JF7L7F7FJ...........
.....|FJF7...L7|||F7F7LJL--7F7L7L7|F7L7L-J|||FJ|F-7LJF-7L-7F7FJF-J..FJ|FJ||F-J...L-7FJF7|LJLJL-7L7|L7L7F---7L---JL-------JL7|||L-7..........
....FJL-JL7..FJLJLJLJ|.F7F7LJL7L-JLJL-J.F7|LJL7|L7L--J.|F7LJLJFJF7F7L7||FJ||.......LJFJLJF--7F7L-JL7L7LJF7.L7F---7F7F---7F-JLJ|F7|..........
....L7F7F7L7FJF-7F--7|FJLJL7F7L---------JLJF-7LJFJF7.F7LJL----J.|||L7||||FJ|F7.....F7L---JF7LJL-7F7L-JF-JL7FJL-7.LJ|L--7|L7F7.LJLJF-7.......
....FJ|LJL7LJFJFJ|F-JLJF--7LJL---------7F7FJ.|F7L-JL-JL-----7F7FJ|L7||||LJFJ|L-7..FJL---7FJL7F-7|||F--JF7FJL7F7|F7.L-7FJL-J|L7F7.FJFJ.......
....L-J.F-JF7L7L7|L7F7FJF-JF7F7F-----7.|||L7FJ||F-7F7F7F----J||L7|.||||L7FJ.|F7|F7L----7|L7FJL7LJ|LJF--JLJF7LJ|LJL--7|L7.F-JFJ|L7|FJ........
.......FJF7|L7|.LJFJ|||FJF7|||||F---7L7LJ|FJL7|||.||LJLJ.F7F7||FJL-J|||FJL7FJ|LJ||..F7.|L-JL-7L--JF7L----7|L-7L-7F7FJL7L7|F-JFJFJ||F7.......
.......L-J|L7||F--JFJLJL-JLJLJLJL--7|FJF7LJ.FJ||L7|L-7F--JLJLJ||F-7FJ||L-7|L7L7FJL7FJL7L---7FJ.F7.|L-7F-7LJF-JF7LJ||F7L7LJL--JFJ.|LJL---7...
..........|FJ||L---J.F7.F7F-7F-7F--JLJFJL--7|FJ|FJ|F7|L7F7F7F7|LJFJL7||F7|L-JFJ|F7|L-7L-7F7||F-JL7L-7LJFJF7L7.|L-7|LJL7|F7F---J.FJF-7F7FJ...
..........LJ.LJF7F---JL7||L7|L7|L-----JF7F-JLJ.LJ.LJLJFJ|||LJLJF7L-7||LJ|L7F-J.||LJF-JF7LJLJ|L7F7L-7L-7L-JL7L-JF-JL-7FJLJ|L---7FJFJ.LJ||....
.............F-J|L----7||L-JL-JL--7F7F-J|L------7F7F7.L-J|L-7F-JL--JLJF7|FJL-7FJL7.L--JL---7L7LJL-7|F7L--7FJF--J.F-7LJF-7L----JL7L-7..LJ....
.............L-7|F7F--JLJF7F7F-7F7LJLJF7L----7F7||LJ|F7F-JF-JL------7FJLJL-7FJL7FJF7F7F7.F7L7L7F--JLJL---JL-JF7F7L7L7.|FJ.F7F---JF7L7.......
.............F-JLJLJF7F-7|||LJFJ||F7F7||F7F7.||LJ|F-J||L-7L-7F--7.F7|L7F7F7||..||.|LJLJ|FJ|.|FJL----------7F7|LJL-JFJFJL7FJ||F-7FJL-J.......
.............L------J||FJ|LJ.FJFJLJLJLJLJLJL7LJF7||.FJ|F7|F-J|F7L-J|L7LJ||||L7FJL7|F7F-JL7|FJ|.F7F--------J|||F----J.L7FJL7||L7|L--7........
............F7F7F7.F7LJL-JF-7L-JF----------7L7FJLJL7L7||LJL7.LJL--7L7|F-J|||FJ|F-JLJ|L-7.||L7|FJ|L--------7|LJL---7F7FJL7.|||FJL---J........
.........F--JLJLJL-JL-----JFJF7.L---------7|FJL---7L-J|L--7L-7.F7.L7||L-7|||L7||F7F-JF-JFJL-J||FJ.F7F7F---J|F-----J|LJF-JFJLJL-7F7.F7F7.....
.........L--7F-7F---7F7F--7L-JL7F----7F7F-J||F-7F7|F-7L-7.L-7|FJL--J||F-J||L7LJLJ|L-7L-7L7F-7|||F7||||L7F7.|L-7F7F7|F-JF-JF-7F7LJ|FJ|||.....
...........FJL7||F--J|LJF7L---7|L---7||LJF7LJL7LJ||L7L--JF7FJ|L-7F-7LJL--JL7L-7F-JF7L7FJ.LJFJ||LJ||||L7LJL-JF7LJLJLJL--JF-J.||L--J|FJ||.....
...........L7FJLJL---JF-JL----J|F7F-JLJF7|L--7|F-J|FJF7F7||L7|F7LJ.L--7F-7FJF-J|F-J|FJ|..F7L7LJF-J||L7L---7FJ|F--7F--7F7L--7LJF---JL-JL7....
..........F7LJF7F----7L--7F7F-7|||L7F-7|||F--J|L7FJL7|||LJ|.||||F7F--7|L7LJ.L-7LJF-JL7L7FJL7L7FJ.FJ|FJF--7LJFJL-7|L-7||L7F7L--JF7F7F7F-J....
.........FJL7.||L---7L7F7||LJFJLJL-J|FJ||||F-7L7|L7FJ||L7FJFJLJLJ|L7FJ|FJ.F---JF-JF7.L7|L-7|.||F7|FJL-JF7L-7L---JL--JLJFJ|L7F--JLJ||LJ......
.........L-7L-JL---7L7LJ|LJF7L7F----JL-JLJ||FJFJL7|L7||FJ|.L-7F--JFJL-J|F7L---7|F7||F7||F-JL7|||||L-7F7|L--JF--7F7F7F7.L7L7|L----7||........
...........L7F--7F7L-JF7L7FJL-J|.F7F7F7F--J|L7L-7LJFJ|||FJF7FJL7F7L-7F-J|L7F7.||||||||||L7F7||LJ||F-J||L--7.|F-J|||||L-7|FJL-----JLJ........
............||F-J|L-7FJL7LJF7F7|FJLJLJLJ.F7|FJF-JF-J.||||.||L7FJ||F7|L-7|FJ||FJLJLJ|||||.LJ||L7FJ|L7FJL---JFJL--JLJLJF7||L-7F7..F7..........
............LJ|F-JF-J|F7L-7|LJ||L-----7F7|||L7L-7|F-7||||FJL-J|FJ||LJF-J|L7||L-7F--J|||L7F7|L7|L-JFJL------JF7F------JLJ|F7LJL--JL7.........
............F-J|F-JF7||L--J|F-J|F-----J||||L7|F7||L7||||||F---JL7|L7FJF7L7|||F7||F7.|||FJ||L7|L7F-JF7F7F7F-7||L--------7LJL7F----7L7........
............L-7|L--JLJL7F-7|L--JL--7F7FJLJ|FJ||||L7|||||||L--7F7|L7||FJL7|||||||LJ|FJ|||FJL7||FJ|F7|LJ|||L7LJ|F--7F7F7FJ.F7|L---7|FJ........
..............||F--7F--J|FJ|F-7F---J||L--7LJ.LJLJFJ||||||L7F-J|LJ.||LJF-J|||LJ|L-7|L7||||F-J|||FJ|||F-J|L7L7.|L7.LJLJLJF-J|L--7FJ|L7........
..............LJL-7|L---JL7|L7|L7F-7||F-7L7F-----JFJLJLJ|FJL-7L--7|L-7|F7|||F-JF7||FJ||LJL7FJ|||FJ||L-7L7L7L7L7L---7.F-JF-JF-7|L7L-J........
................F-JL7.F7F7||FJL7LJFJ|||.L-JL---7F7L-7F--J|.F7|F--JL7FJ||||||L7FJ||||FJL-7.||FJ||L7|L7FJFJ.|FJ.L-7F7L7L7FJF-JFJ|FJ...........
................L--7L7||||LJ|F7|.FJFJ|L----7.F-J|L7FJ|F-7|FJLJL--7FJL7||||||FJL7||LJ|F7FJFJ||FJL-J|FJL-JF-JL---7||L7L-JL-JF7|.LJ.F7F7.......
...................L7LJLJ|F7LJ||FJFJFJF7F-7L7L7FJFJ|.LJFJ||F7F7F-J|F-J||LJ||L7FJ|L7FJ||L7|FJ|L7F7.|L7F--JF-7F7FJ|L7|F-7F--JLJF-7FJLJL-7.....
..............F--7F7L---7LJL7FJ|L7|.L7|LJ.L7|FJ|FJFJF7FJFJ|||||L-7|L7.LJ.FJ|FJ|FJFJL7||FJLJ.L7||L-JFJ|F-7|.|||L7L7|LJFJL---7.L7||F--7FJ.....
..............L-7LJL----JF-7|L7L-JL-7|L-7F7LJL7|L7|FJLJFJFJ||||F7|L7L---7|FJL7|L7|F7|||L---7.||L7F7|.LJFJL7LJ|FJ.|L-7L7F7F7L--JLJ|..LJ......
................L-------7|FJL-JF7F7FJL-7LJL--7|L7LJL--7|FJFJ|||||L7|F---J||.FJL7|||LJ||F--7|FJ|FJ|||F--JF7L7FJL-7|F7|FJ||||F-7F-7L-7........
................F------7LJL7F7FJ|||L--7|F-7F-JL-JF----J|L7|FJ|LJ|FJ|L--7FJL7|F7|||L7FJ|L-7||L7|L7|LJL7F7|L-JL7F-JLJLJL7||||L7||FJF-J........
................L-----7L---J|||FJ||F7FJ|L7|L--7.FJF-7F7L7||L7L7.|L7|F7FJL7FJ||LJLJFJL7|F7||L7||FJ|F--J||L---7|L---7F7FJ||||FJ||L7L7.........
..............F7F7F7F7L----7||||FJ||LJ.L7||F--JFJFJFJ|L7|||FJFJFJFJ|||L7FJ|.||.F--JF-J||LJ|FJ||L7|L-7FJ|F---JL7F-7LJ||FJ||LJ.LJ.|FJ.........
..............|LJLJLJL-----J|||||.|L--7FJ||L--7L-J.|FJFJ|||L-JFJFJFJ|L-JL7L7||FJF7FJF7||F-J|FJL7LJF-J|FJL7F--7|L7|F-JLJ.||....F-JL7.........
..............L-7F---------7||||L7|F--JL-JL-7FJF---JL7L-J||F--JFJ.L7L---7L7|||L7|||.|LJ|L-7||F7|F7|F7|L-7LJF7||FJ|L----7|L7..FJF--J.........
................LJ.F7F-----J|||L-J|L------7FJ|FJF7F--JF--J||F7FJF7.|F7F7L7||LJ.||LJFJF7L7.||LJ|||LJ|||F-JF-JLJ|L7|F7F7FJ|FJ..L-J............
.................F-JLJF7F--7||L-7FJF7F7F--JL-JL7||L-7.L-7FJ|||L-J|FJ|||L7|LJF--JL7FJFJ|FJFJ|F7LJL-7|LJL-7L-7F-JFJLJLJ|L7||..................
................FJF7F7|||F-J||F7||FJ|||L---7.F7LJ|F7L-7FJL7|||F--JL7|||FJ|F7|F-7FJL7L7||FJFJ||F7F-JL-7.FJF-JL-7L--7.FJFJ||..................
................L7|LJ|||||F7||||||L7|||F-7FJFJL--J||F-JL7FJLJ||F--7LJ||L7LJ|||FJ|F-JFJ||L7|FJLJLJF-7FJFJFJ.F-7L7F7L7|FJ.LJ..................
.................LJF-J|||||||LJ|||FJ||||.|L7|F-7F7|||.F-JL-7FJLJF-JF7LJ.L-7|LJ|FJ|F-J.||FJ|L7F7F-JFJ|.L7|F7|FJ.|||FJ||......................
...................L-7||||||L7.LJ||FJ||L7L-J||FJ|||||FJF--7||F-7L--JL-7F--JL-7|L7||...|||FJ.LJ||F-JFJF-JLJLJL-7||LJ.|L-7....................
.....................||LJ||L7L7F7LJL7|L7L-7FJ|L-J|||||FJF-J|||FJF-7F7FJL--7F7|L-JLJ...LJ||F---J||F7L7L7F7F7F7FJ|L7..L7FJ....................
.....................LJ.FJL7L7LJL--7LJ.L7FJ|FJF--J||||L7L-7|||L7|.||LJ....LJLJ..........LJL---7||||FJ.||||||LJ.|FJ...LJ.....................
........................L7FJFJF-7F7|....||.|L7L---JLJL7L-7||||FJL7|L--7....................F--J||||L-7LJ|||L7..||...........................
.......................F-JL7L7L7LJ|L-7..||FJFJF-------JF-J|||||F7||F-7L7...................L-7FJ|||F-J..|||FJ..LJ...........................
.......................L7F7|.L7L7.L--J..|||FJFJF7F7F7F7L-7|||||||||L7L-J.....................||FJ||L-7..LJ||................................
........................||LJ..L7L-7.....LJLJFJFJ|||||||F7|LJLJLJLJL-J.......................FJ||FJ|F7|....LJ................................
........................LJ....FJF-J.........L-J.||||||||||..................................|FJ||.LJLJ......................................
..............................L7L7..............LJ||||||||...............................F--JL7LJ...........................................
...............................L7L7...............LJLJ||LJ...............................L----J.............................................
................................L-J...................||....................................................................................
......................................................LJ....................................................................................
............................................................................................................................................
............................................................................................................................................
............................................................................................................................................
............................................................................................................................................
```

Now, to get the part that is furthest from the start in the loop, I simply have to divide the length of the loop by 2! (This is pretty trivial, think of it as the "opposite side" of the loop)

## Part 2
---

This is the part that was the most difficult. My initial idea was to simply cover every tile outside of the loop and subtract the number of total tiles by the number of tiles outside.
This would have worked if we didn't have to be able to squeeze through pipes.

A good idea here would have been to look at the (even-odd rule)[https://en.wikipedia.org/wiki/Even–odd_rule]. Unfortunately I wasn't so smart lol.

So here is my idea instead:

### Step 1:

I walk alongside the loop and I keep track of tiles that are on my left side and on my right side (with regards to the direction I am heading towards. For example, if I am heading Eastbound, the my left side is North and my right side is South). I keep track of both sides on two different lists.

By definition, a loop has an enclosed part and an outside part. If my intuition is correct, if you walk alongside the loop, the enclosed part is always going to be on the same side at all time. It might be the left side or the right side depending on where you start from and what direction you head towards first, but it will always be the same.

With a small sample, here is what I get:
```
..........
.S------7.
.|F----7|.
.||....||.
.||....||.
.|L-7F-J|.
.|..||..|.
.L--JL--J.
..........
```

I get the following borders (l is left border and r is right border):
```
.llllllll.
lF------7l
l|F----7|l
l||llll||l
l||llll||l
l|L-7F-J|l
l|rr||rr|l
lL--JL--Jl
.llllllll.
```

(I won't display the border for my input here, as it is pretty big, but you can run my code if you want!)

Here it is quite obvious that the outside part of my loop is on the left side (and in fact, due to how I choose my starting point for walking along the loop, it is pretty much guaranted to be the left side. That is because my starting point is always going to be the uppermost and leftmost '-' tile.)

### Step 2:

Despite that I am pretty much always guaranted to have the left side as the outside part of the loop, I want to be safe and check that this is the case. To do so, I want to cover the outside of the loop (without squeezing through pipes). For this, I simply walk in a (breadth-first way)[https://en.wikipedia.org/wiki/Breadth-first_search] starting from a point that is guaranted to be outside of the loop.

I choose the point (0, 0) as that starting point, as my grid starts at (1, 1), therefore this point cannot be enclosed. I also "extend" my grid by one tile on each side to make sure that I get every side of the loop (in fact, I also did this while searching for the borders)

With the previous example, here is what I get:
```
OOOOOOOOOO
OF------7O
O|F----7|O
O||....||O
O||....||O
O|L-7F-J|O
O|..||..|O
OL--JL--JO
OOOOOOOOOO
```

To know which side is the enclosing side of the border, I simply now check which side is totally disjoint from the outside part that I juste mapped. Here it is obvious that the left side is not disjoint, while the right side is disjoint, therefore the right side is the enclosing side. Considering that I am actually surounding the loop when doing my bfs traversal, I am guaranted that one of the side won't be disjoint.

Another example:
```
.F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...

.F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJF7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...

lF----7F7F7F7F-7l...
l|F--7||||||||FJl...
l||lFJ||||||||L7ll..
FJL7L7LJLJ||LJrL-7l.
L--JlL7rrrLJF7F-7L7l
llllF-JrrF7FJ|L7L7L7
...lL7rF7||L7|rL7L7|
....l|FJLJ|FJ|F7|lLJ
...lFJL-7l||l||||lll
...lL---JlLJlLJLJl..

OF----7F7F7F7F-7OOOO
O|F--7||||||||FJOOOO
O||.FJ||||||||L7OOOO
FJL7L7LJLJ||LJ.L-7OO
L--J.L7...LJF7F-7L7O
OOOOF-J..F7FJ|L7L7L7
OOOOL7.F7||L7|.L7L7|
OOOOO|FJLJ|FJ|F7|OLJ
OOOOFJL-7O||O||||OOO
OOOOL---JOLJOLJLJOOO
```

### Step 3:
Now that I know which border is on the enclosing side of the loop, I simply need to "expand" the border as much as possible. Once again, I perform a bfs starting from each known point on the enclosing side of the loop (the points on the border). If a point is on the enclosing side of the loop, then its non-pipe neighbours also are on that side.

Examples (I am not putting the previous steps back, you can find them above):
```
OOOOOOOOOO
OF------7O
O|F----7|O
O||OOOO||O
O||OOOO||O
O|L-7F-J|O
O|II||II|O
OL--JL--JO
OOOOOOOOOO
```

```
OF----7F7F7F7F-7OOOO
O|F--7||||||||FJOOOO
O||OFJ||||||||L7OOOO
FJL7L7LJLJ||LJIL-7OO
L--JOL7IIILJF7F-7L7O
OOOOF-JIIF7FJ|L7L7L7
OOOOL7IF7||L7|IL7L7|
OOOOO|FJLJ|FJ|F7|OLJ
OOOOFJL-7O||O||||OOO
OOOOL---JOLJOLJLJOOO
```

Now that I know where my inside tiles are, I simply need to get the number of tiles I found, and I am done!


With my input, from step 1 to 3:
```
.....................................................................lF7l..ll....................ll.llll....................................
.....................................................................l||l.lF7l.......ll.........lF7lF7F7l...................................
...................................llll...........................lllFJL7ll||ll.....lF7ll.......l|L7||||l...................................
..................................lF7F7llll................ll.ll.lF7lL7FJF7||F7ll...l||F7l......lL7||LJ|l...................................
..................................l|LJ|F7F7l......ll.ll...lF7lF7lFJ|F7||FJ||LJL-7l.lFJ|||lllll.lF7|||F7|l...................................
..................................lL-7LJLJL7l....lF7lF7lllFJL-JL7L7||||||FJL-7F-Jl.lL7LJL7F--7lFJLJLJ|LJl...................................
.................................llllL7F---Jl....l||FJ|F-7|F---7L-J||||LJL--7|L--7lllL7F-J|F-JlL--7F-JF7l...................................
................................lF-7F7|L--7lll..lFJ||FJL7|LJF--JF-7LJ||F----J|F--JF7F-J|lFJ|lllF7FJ|ll||l...................................
................................lL7LJLJF--JF-7l.lL7|||F7||F7L7F-JlL-7LJL-7lF7||F--J|L-7|FJFJlF-J||FJll|L7l..................................
.................................lL---7L-7l|FJl.lFJLJLJLJLJ|lLJF7lF-JF---JFJLJLJF7FJF7||L7|F7L-7||L7F7L7|l..................................
..................................lF7FJF-JFJL7l.lL-7F7F7F-7L7F-JL-JF7L7F7lL7F---JLJFJ|||FJLJL7FJLJFJ|L-JL7l.................................
.................................lFJ|L7L7FJF-Jl.lF-J|LJ|L7L-JL---7FJL7LJ|F7|L-7F7F7L7||LJF---JL7F-JlL7F7FJl.................................
...............................lll|FJl|FJL7|lF7llL--JF-JFJlF7lF--J|F7L-7LJLJF-J|LJ|FJ||F-JlF7F7|L-7F-J|LJF7l................................
............................lllF7FJL7FJL7FJL7|L-7F7F7L7FJF7|L7L---J||F-JF---JF-JF7||FJ|L--7|||||F-JL-7L--JL7l....ll.........................
...........................lF-7||L7FJL-7||F7||F-J||||FJL-J||FJF----J|L-7|F7F7L--J||||FJF--J|LJLJL7F7FJF----Jll..lF7l........................
...........................lL7LJL-J|F7FJLJ||LJL7FJ||LJF---J||lL7F7F7L7FJ||LJL--7l||||L7L7F7L-7F-7|||L7L----7F7lll||l.ll.....................
............................lL----7|||L-7FJL---JL7|L-7|F7ll||F7LJLJL7|L7||F----JFJLJ|FJFJ|L-7||l||||FJF----J||F7FJL7lF7l....................
............................lF---7|LJ|F7|L-7F7F7FJL7FJ||L7FJLJ|F7F7FJL-JLJL7F7F7L--7|L7|FJF7LJL7LJ||L7|lF7F7|||LJF-Jl|L7l...................
.........................ll.lL7F7LJF-J|||F-J||||L7FJL7||FJ|F--J||||L--7F7F7LJ|||F--JL-J||FJL-7FJF-J|l|L-J||LJ|L-7L---JFJlll.................
.....................llllF7llFJ|L-7L7FJ||L-7||||l||F-J||L7|L7F7|||L7F-J|LJ|F-J||L-----7LJL-7FJ|FJF7L7|F--J|F-JF-JF7F7FJllF7llll.............
..................lllF--7||F7L-JF7|FJL7LJF-J||||FJ|L-7||FJL7||||LJFJL--JF7||F7|L7lF7F-JF---JL-JL-JL7|||F7FJL7lL7FJLJLJF-7||lF-7l............
.................lF7lL-7|||||lF-JLJL--JF7|F7||||L7L7FJLJL7FJLJ|L-7|F7F-7|LJ||LJFJFJLJF7|lF7F7F7lF7FJLJ||LJF-JF7||F7F-7|FJ|L7L7|l............
.................l||F7l|LJLJL7L--7F7F-7|LJ|||LJL7|FJL7F--JL7F7|F-JLJ|L7|L-7LJF7L7L---J|L-JLJ||L7||L-7FJ|F-JF7|||LJLJFJ||FJFJFJL7l...........
................lFJLJL-JF7F-7L7F7LJLJFJ|F-J||F--J|L7FJL7lF7||LJL--7FJFJL--JF-JL-JlF---JF7F--J|FJ|L7FJL-JL-7|||LJF--7L7||L7L7|F7|l...........
...............lFJF-----JLJl|FJ||F7F7L7LJF7LJ|F--JFJL7FJFJ||L7lF7FJL7L-7F-7L7F---7L----J|L7F7|L7L7|L7F7F--J|||F-JF7L-J||lL7|||LJl...........
...............lL-JlF7F--7F-J|FJLJ|||lL7FJ|F-JL--7|F7||l|FJL7L7||L-7L--JL7L7||F--JF7F---JFJ||L7|FJL-J|LJF7l|LJ|F-JL--7||F7|LJL--7l..........
...............lF7lFJ|L-7|L-7|L--7LJ|F7||l|L-7F-7|||LJL7||F7L7||L7lL---7FJl|LJL-7l||L---7L-J|FJ|L---7L7l|L7L-7LJF----J|||||F----Jl..........
.............llFJL7L7|F7||F-JL7F7L-7||LJL7L--J|FJ||L-7FJ||||l|||FJF7F7lLJF7|F---JFJ|F7F7|F--J|FJF--7L7L-JFJF7|F-JlF--7|LJ|||F7F7ll..........
............lF-JF7L7||||||L-7FJ|L7FJ||F--JF7F7||FJL7FJL-JLJL7|LJL7|LJ|F7FJLJL---7|FJ|LJ|||lF7|L7|F-JFJF--JFJLJ|lF7|F7|L-7LJLJLJ|F7l.........
............l|F-JL7|||||||F7|L-JFJL7|||F-7|||LJ||F-JL-7F----JL-7FJL-7||LJF-7F7F7||L7L-7LJL7|||FJ|L-7L7L7F7|F--JFJ|||LJF7|F7F--7LJL7l........
............lLJF--JLJ||||||LJF-7L--J||||FJ||L-7|||F7lFJ|F7F7lF-JL7lFJ||F-Jl||LJLJ|FJF7L7F-J|LJL7|F7|FJFJ||||lF-JFJ|L-7||LJ||F7L---Jl........
.............llL----7|||||L-7|lL---7||||L7|L-7|||LJL7L7||||L7L-7FJFJFJLJF7FJL---7||FJ|FJL-7L-7FJLJ||L7|FJLJL7|F-Jl|F-J|L7l|LJL-7F--7l.......
............llF-----JLJLJ|F7LJF7F7l||||L7||F-J||L--7L-JLJ||FJF-JL-JFJF7FJLJF----J||L7|L7F-JF7|L-7l||FJLJF---J||lF-JL-7L7|FJF7F-J|F-Jl.......
...........lF7L--7F7F-7F7LJL--JLJ|FJLJL7|LJL7FJL-7lL-7F--J|L7L----7L7||L--7L-7F7FJL7||FJL-7|||F-JFJ|L-7FJF7F7|L-JF---JFJLJFJ|L--JL7l........
..........lFJL--7LJLJlLJL7F7F---7|L---7|L-7FJ|F--JF7l||lF7|FJF7lF7L7|||F7l|F-J||L7FJ||L7F-J||||F7L7|F7||FJLJLJF--JF7F7|F7FJlL7F-7FJlll......
..........lL---7L7F-7F---J|LJF--JL7F-7|L7FJ|FJ|F7FJL7|L7|||L7|L7|L7|LJ|||FJ|F7||FJL7||FJ|F7|||||L7|LJ||||F---7L7F-J|||||LJF7l||lLJF7F7l.....
...........llllL7LJFJL----JF7L7F-7LJr|L7||FJ|FJ|||F-JL7||LJr||r||FJ|F-J|||rLJ||||F-J|||r||||||||r|L7FJ||||F7lL7LJF-J|LJL7FJL7|L7F7|LJ|l.....
............llF7L-7|F7lF---JL-J|FJF-7|FJLJL-J|FJLJL7F7||L-7FJL7LJL7||F7||L-7FJ|LJL7FJLJFJ|||LJLJFJFJL7|LJLJL7lL--JF7|F-7LJF7||FJ|||F-Jl.....
...........lF-JL--JLJ|FJF------JL7|FJLJF7rF--JL--7r||LJ|F7||F-JF--J|||LJ|F7|L7|F--JL--7L-J|L-7F-JFJrrLJF----JF7F--JLJL7L7FJLJ|L-JLJ|ll......
...........lL-------7|L-JF7F--7F-J|L---J|FJF-7F7FJFJL-7|||||L7FJF-7||L7FJ|LJrLJL7F7F--JF--JF7||F7L-7F--JF----JLJF7F---JlLJF-7L--7F7L-7lll...
.........lllF7F----7||lF7||L7FJL--JF-7F7||FJr||LJFJF7FJ|||||FJL7|rLJL7|L7L---7F-J||L--7L--7|||LJL7FJ|F-7|F-----7||L-7F7F7l|FJF7l|||F7L--7l..
........lF--J|L---7LJL-JLJL7||F-7F-JrLJLJLJrFJL-7|FJ|L7|||||L7FJL--7FJ|FJF7F-J|F7||F-7L7F-J||L-7FJ|rLJrLJL7F7F7LJ|F7LJLJL-JL-JL7||LJ|F--Jl..
........lL--7L----JF7F7F7F7LJ|L7|L---------7L7F-JLJrL7||||||FJ|F7F-JL7||FJ|L7FJ|||||FJFJ|F7||F7|L7|F----7FJ|LJ|F7LJ|F-7F-------JLJllLJlll...
.......lF---JF----7|LJLJLJ|F7L-JL----7F7F-7|FJL-----7|||||||L7||LJF7FJ||L7L-JL7||||||FJrLJ||||||FJ|L---7|L-JF7LJL-7LJl|L--7lF7lF7l..ll......
.......lL-7F7|F---JL7F----J||F7F-7F-7|||L7LJL7F7F---JLJLJ||L7|||F-JLJFJ|FJF---J|||||||F---J|LJLJL-JrF--JL---J|F-7FJF-7L7F7L-JL7||l..........
........llLJLJL7F---JL7F-7FJLJLJlLJFJLJL-JrF-J||L----7rF7LJrLJLJL--7FJr||r|F7F7|LJ||||L-7F7|F---7F-7L-------7LJlLJl|FJl|||F7F7LJ|l..........
..........lF---J|F--7lLJlLJF7F7F7F-JF7F---7L--J|F---7L-JL-7rF7F----J|F-JL7LJLJLJF-J|LJrFJ||||F-7|L7L7F7F----JF7F---JL-7LJLJLJL--Jlll........
..........lL-7F7|L-7|F7lF7FJLJLJ|L--JLJF--JF--7|L-7r|F7F-7|FJLJF-7F7|L7F7L-----7L--JrF7L-JLJLJFJL7L7|||L---7l||L7F7F--JlF7F7lF7F---7l.......
.........lF--J|||F-J|||FJLJF---7|F--7F7|F-7L7FJL-7L7|||L7||L7F7L7|||L7LJL7F-7F7L-7F--JL-7F7F7FJF7L-JLJ|F--7|FJ|FJ||L-7F7|LJL7|LJF7FJl.......
.........lL---JLJL-7|||L-7FJlF-JLJF7LJLJ|FJFJL--7L7||||FJLJrLJL7|||L-JF7FJ|rLJ|F-JL7F--7||LJLJFJL----7|L-7LJ|FJL7|L--J|LJF-7LJF-JLJl........
.........lF-7F7lF7FJLJL-7LJF7L-7F-JL--7FJL-JF---Jr|||||L-7F7F--J|||rF-J|L7L--7|L7rrLJF7||L---7|F7F---JL-7L--JL--JL7lF7|F-Jl|F7L---7l........
.........lL7|||FJLJF-7F7L--JL--J|F---7LJF7F7L----7||LJL--J|LJF--JLJFJF7L-JF7FJL7L7F--JLJL----JLJ|L-----7L-7F7F7F-7L-JLJL--7LJL---7|l........
..........l|LJ|L7F7L7LJL7F-7F7F7|L-7rL--JLJL7F---J|L7F7F7rL-7L7F---JFJ|F-7|||F-JFJ|F--7F-----7F7|F-----JF7||LJLJl|F7F7F---JF7F7llLJl........
.........lFJF7L-J|L7|F--J|FJ|LJLJF-JF-----7rLJF--7L-J|||L-7r|FJ|F7F7|FJ|r||||L-7L7||F-J|F----J|LJL-----7||LJF7F-7LJLJLJlF--JLJL--7lll.......
.........lL-JL---JlLJL---JL7|F---JF7L7F---JF7r|F-JF7r|||F-JFJL7|||||||FJFJ||L7FJFJLJL7FJL7F7F7|F------7LJL-7|||FJlF7F7F7|F-------JlF7ll.....
..........llllF----------7lLJL7F--JL-JL----JL7|L--J|FJLJ|F7L--J||LJ||||rL7|L7|L7L-7F-JL7rLJ||||L7F-7F7L-7F7LJLJL--JLJLJLJL7F7F-7F7l||F7l....
.............lL7F7F7F--7FJF7F7LJF-7F7F7F---7FJ|F---JL7F-J||rF-7LJF-J|LJrr||r||FJF7||F7FJF--J|||FJL7LJ|F7LJL--7F7F-7F------J|||FJ||FJLJ|l....
............lF-J|||LJF-JL-JLJ|F7L7LJ|||L--7LJFJL7F7rFJL--JL-JFJF-JF7L7r.rLJr||L7|LJLJ|L7|F-7|LJL7FJF-J|L-7lF7LJLJl|L-------JLJL7|||F--Jl....
.........llllL-7|LJlFJF--7F-7LJL7L-7LJ|F-7L--JF7LJL-JF7F7F---JrL--JL7|r..rrr||r|L--7FJFJLJr||F-7||rL--JF7L-JL-7F-7|F7F7F7F-----J||||F7l.....
........lF-7llFJL7F-JFJF7LJlL--7|F-JF7LJrL--7FJL7F---JLJLJrF7F7F7rrr||r....rLJFJF7FJL-JrrF7LJL7LJ|F----JL-----J|FJLJLJLJ|L------JLJLJ|l.....
........lL7|llL--J|F7|FJL-----7||L--JL7rF-7r|L-7|L---------JLJ|||F7rLJr.....rrL7|||F7rF--JL7F7L-7|L--7F-----7F-JL-7F7F-7|F7F-7F-7F7F-Jl.....
........lFJ|lF7lF7LJLJL------7LJL-----JFJFJFJF-J|F7F7F-7F7F7F7LJLJ|rrr........r|||LJL7L---7LJ|F7LJrF7LJF----J|F---J|LJFJLJLJlLJlLJ|L7l......
........lL7L7||FJL7lF----7lF7L---------JFJFJFJrFJ|LJLJr||LJ||L7F-7|r.........rFJ|L-7FJrF-7L-7LJL---JL-7|F7F--JL----JF7L7lF7lF----7L-Jll.....
.........l|FJ||L-7|FJF--7|FJL-----7F7F7FJrL-JF7L7L7F7F7|L-7||FJL7LJr.........rL-JrrLJrrL7L-7|F-7F----7|LJLJF7F------JL-JFJL-JF---JlF7F7l....
........lFJL-J|F7|LJFJF-JLJF-7F7F7LJ||LJrF7F-JL7L7LJLJ||F-JLJL-7L-7r..........rrr..rrrF-JF7LJ|FJ|F7rFJL----J|L-7F-7F----JF---JlF-7FJLJL7l...
........lL--7FJ||L-7|FJF7F-JlLJLJL-7||F--JLJF-7L7L7F--JLJF7rrrFJF-Jr............rr...r|F-JL7FJL-J|L-JF-7F--7|F7LJFJ|F----JF7lF7|FJL7F--Jll..
........lF--JL-JL--J||FJLJF7F7F7F7FJLJ|F7F-7L7|FJFJL----7|L7rrL7L7r............rF7r..rLJF7rLJF7rFJF--JrLJF-J||L7FJFJL----7|L-J||L7FJL----7l.
.......lFJF7F-7F7F--JLJF--JLJLJLJ|L---J||L7L-JLJrL7F----J|FJrrFJFJr............r||rrrrrFJL---JL7L-JF7F7F7L7lLJlLJl|F-----J|F--J|FJL7F7F--Jl.
.......lL-JLJlLJLJF7F-7L7F------7|lF---JL-JF--7rF7LJF7F7r|L7rrL-Jr............rFJ|rF7F-JF---7F7L---JLJ||L-JF7F7F7FJL---7F7||F--JL-7||LJlll..
........llF-----7l||L7|FJ|F-----JL-JF----7rL-7L7||rFJLJL7|FJrrrrr.............r|FJFJ|L-7|F--J|L7F7F--7|L---JLJLJLJF----J|||LJF-7F7LJL7l.....
.........lL----7L-JL-J|L7|L7F--7F7F-JF---JF-7L7||L7|F---J||rF7r...............r|L-JFJF7LJL---JrLJLJF7|L---------7FJF--7l|||F-JlLJL-7FJl.....
.........lF----JF----7|l||FJ|F-J|||F7|F7rFJFJFJ||FJ||F7F-JL-J|r...............r|F-7L-J|rF7F7F7F7F7FJLJF7F7F7F--7LJFJF7L7|LJL----7F7||l......
.........lL-7F7FJF---JL7LJL-JL--JLJ|LJ|L7L7L7L7||L-JLJLJF7F-7|r...............rLJrL7F7L7|LJLJLJLJLJF--JLJLJ||F-JF-JFJL7LJF7F7F--J||LJl......
..........llLJLJlL----7L----7F7lF7FJF-JFJr|FJFJLJF------JLJr||r................rr.rLJL7LJF-------7FJF7F7rF-J||F7|F-JlFJF7|LJLJlF7|L7l.......
...........lF------7F7L----7LJL-J|L-JF7L--JL-JF-7L-7F7F7F--7LJr....................rF-JF7|F----7FJ|FJLJL7L--JLJLJL--7L-J|L--7F7|LJFJl.......
...........lL-7F7F7LJL----7L----7L---JL---7F7FJFJF7LJLJLJF7L7r.........rr..........rL-7|LJL7F-7LJFJL7F-7L-7F7F7F7F--JF-7L7F7LJ||F7L-7l......
...........lF-J|LJL7F7F-7FJF7F-7L----7F--7|||L7L7|L7F-7F-JL-Jrr.......rF7r.......rrF7FJ|F7r|L7L7FJF-J|FJF7LJ|||||L--7|FJlLJ|F7|||L-7|l......
...........lL7FJF7lLJLJl||FJLJFJF---7LJF-JLJL-JrLJr|L7LJF---7F7r.....rr||r......rF-JLJFJ||FJFJrLJrL7FJL-JL7lLJ||L7F7LJL7lF-J|LJ|L-7LJl......
.....lll....lLJl||F-----J|L--7L7L--7L-7L7rF7rF7F7F7L-JrFJF-7||L7rr..rF7|L7rrrr..rL-7F-JFJ|L-JrF--7rLJrF---JF-7||lLJ|F-7L7L-7L--JF-Jll.......
....lF-7ll..lF-7||L---7F7L---JFJF-7L-7L7L7||FJ||LJL7F--JFJrLJL7L-7r.r||L7|F--7r.rF-JL7rL7|F7F7L-7L7F-7L----JFJLJF7lLJlL-JF7L7F7FJll.........
....l|FJF7l.lL7|||F7F7LJL--7F7L7L7|F7L7L-J|||FJ|F-7LJF-7L-7F7FJF-JrrFJ|FJ||F-Jr.rL-7FJF7|LJLJL-7L7|L7L7F---7L---JL-------JL7|||L-7l.........
...lFJL-JL7llFJLJLJLJ|lF7F7LJL7L-JLJL-JrF7|LJL7|L7L--Jr|F7LJLJFJF7F7L7||FJ||rr...rrLJFJLJF--7F7L-JL7L7LJF7lL7F---7F7F---7F-JLJ|F7|lll.......
...lL7F7F7L7FJF-7F--7|FJLJL7F7L---------JLJF-7LJFJF7rF7LJL----Jr|||L7||||FJ|F7rr..rF7L---JF7LJL-7F7L-JF-JL7FJL-7lLJ|L--7|L7F7lLJLJF-7l......
...lFJ|LJL7LJFJFJ|F-JLJF--7LJL---------7F7FJr|F7L-JL-JL-----7F7FJ|L7||||LJFJ|L-7rrFJL---7FJL7F-7|||F--JF7FJL7F7|F7lL-7FJL-J|L7F7lFJFJl......
...lL-JlF-JF7L7L7|L7F7FJF-JF7F7F-----7r|||L7FJ||F-7F7F7F----J||L7|r||||L7FJr|F7|F7L----7|L7FJL7LJ|LJF--JLJF7LJ|LJL--7|L7lF-JFJ|L7|FJl.......
....lllFJF7|L7|lLJFJ|||FJF7|||||F---7L7LJ|FJL7|||r||LJLJrF7F7||FJL-J|||FJL7FJ|LJ||rrF7r|L-JL-7L--JF7L----7|L-7L-7F7FJL7L7|F-JFJFJ||F7llll...
......lL-J|L7||F--JFJLJL-JLJLJLJL--7|FJF7LJrFJ||L7|L-7F--JLJLJ||F-7FJ||L-7|L7L7FJL7FJL7L---7FJrF7r|L-7F-7LJF-JF7LJ||F7L7LJL--JFJl|LJL---7l..
.......lll|FJ||L---JlF7lF7F-7F-7F--JLJFJL--7|FJ|FJ|F7|L7F7F7F7|LJFJL7||F7|L-JFJ|F7|L-7L-7F7||F-JL7L-7LJFJF7L7l|L-7|LJL7|F7F---JlFJF-7F7FJl..
.........lLJlLJF7F---JL7||L7|L7|L-----JF7F-JLJrLJrLJLJFJ|||LJLJF7L-7||LJ|L7F-Jr||LJF-JF7LJLJ|L7F7L-7L-7L-JL7L-JF-JL-7FJLJ|L---7FJFJlLJ||l...
..........lllF-J|L----7||L-JL-JL--7F7F-J|L------7F7F7rL-J|L-7F-JL--JLJF7|FJL-7FJL7rL--JL---7L7LJL-7|F7L--7FJF--JlF-7LJF-7L----JL7L-7llLJl...
............lL-7|F7F--JLJF7F7F-7F7LJLJF7L----7F7||LJ|F7F-JF-JL------7FJLJL-7FJL7FJF7F7F7rF7L7L7F--JLJL---JL-JF7F7L7L7l|FJlF7F---JF7L7lll....
............lF-JLJLJF7F-7|||LJFJ||F7F7||F7F7r||LJ|F-J||L-7L-7F--7rF7|L7F7F7||rr||r|LJLJ|FJ|r|FJL----------7F7|LJL-JFJFJL7FJ||F-7FJL-Jl......
............lL------J||FJ|LJlFJFJLJLJLJLJLJL7LJF7||rFJ|F7|F-J|F7L-J|L7LJ||||L7FJL7|F7F-JL7|FJ|rF7F--------J|||F----JlL7FJL7||L7|L--7l.......
.........lllF7F7F7lF7LJL-JF-7L-JF----------7L7FJLJL7L7||LJL7rLJL--7L7|F-J|||FJ|F-JLJ|L-7r||L7|FJ|L--------7|LJL---7F7FJL7l|||FJL---Jlll.....
........lF--JLJLJL-JL-----JFJF7lL---------7|FJL---7L-J|L--7L-7rF7rL7||L-7|||L7||F7F-JF-JFJL-J||FJrF7F7F---J|F-----J|LJF-JFJLJL-7F7lF7F7l....
........lL--7F-7F---7F7F--7L-JL7F----7F7F-J||F-7F7|F-7L-7rL-7|FJL--J||F-J||L7LJLJ|L-7L-7L7F-7|||F7||||L7F7l|L-7F7F7|F-JF-JF-7F7LJ|FJ|||l....
.........llFJL7||F--J|LJF7L---7|L---7||LJF7LJL7LJ||L7L--JF7FJ|L-7F-7LJL--JL7L-7F-JF7L7FJrLJFJ||LJ||||L7LJL-JF7LJLJLJL--JF-Jl||L--J|FJ||l....
..........lL7FJLJL---JF-JL----J|F7F-JLJF7|L--7|F-J|FJF7F7||L7|F7LJrL--7F-7FJF-J|F-J|FJ|rrF7L7LJF-J||L7L---7FJ|F--7F--7F7L--7LJF---JL-JL7l...
.........lF7LJF7F----7L--7F7F-7|||L7F-7|||F--J|L7FJL7|||LJ|r||||F7F--7|L7LJrL-7LJF-JL7L7FJL7L7FJrFJ|FJF--7LJFJL-7|L-7||L7F7L--JF7F7F7F-Jl...
........lFJL7l||L---7L7F7||LJFJLJL-J|FJ||||F-7L7|L7FJ||L7FJFJLJLJ|L7FJ|FJrF---JF-JF7rL7|L-7|r||F7|FJL-JF7L-7L---JL--JLJFJ|L7F--JLJ||LJll....
........lL-7L-JL---7L7LJ|LJF7L7F----JL-JLJ||FJFJL7|L7||FJ|rL-7F--JFJL-J|F7L---7|F7||F7||F-JL7|||||L-7F7|L--JF--7F7F7F7lL7L7|L----7||ll......
.........llL7F--7F7L-JF7L7FJL-J|lF7F7F7F--J|L7L-7LJFJ|||FJF7FJL7F7L-7F-J|L7F7r||||||||||L7F7||LJ||F-J||L--7l|F-J|||||L-7|FJL-----JLJl.......
...........l||F-J|L-7FJL7LJF7F7|FJLJLJLJrF7|FJF-JF-Jr||||r||L7FJ||F7|L-7|FJ||FJLJLJ|||||rLJ||L7FJ|L7FJL---JFJL--JLJLJF7||L-7F7llF7ll........
...........lLJ|F-JF-J|F7L-7|LJ||L-----7F7|||L7L-7|F-7||||FJL-J|FJ||LJF-J|L7||L-7F--J|||L7F7|L7|L-JFJL------JF7F------JLJ|F7LJL--JL7l........
...........lF-J|F-JF7||L--J|F-J|F-----J||||L7|F7||L7||||||F---JL7|L7FJF7L7|||F7||F7r|||FJ||L7|L7F-JF7F7F7F-7||L--------7LJL7F----7L7l.......
...........lL-7|L--JLJL7F-7|L--JL--7F7FJLJ|FJ||||L7|||||||L--7F7|L7||FJL7|||||||LJ|FJ|||FJL7||FJ|F7|LJ|||L7LJ|F--7F7F7FJlF7|L---7|FJl.......
............ll||F--7F--J|FJ|F-7F---J||L--7LJlLJLJFJ||||||L7F-J|LJl||LJF-J|||LJ|L-7|L7||||F-J|||FJ|||F-J|L7L7l|L7lLJLJLJF-J|L--7FJ|L7l.......
.............lLJL-7|L---JL7|L7|L7F-7||F-7L7F-----JFJLJLJ|FJL-7L--7|L-7|F7|||F-JF7||FJ||LJL7FJ|||FJ||L-7L7L7L7L7L---7lF-JF-JF-7|L7L-Jl.......
..............llF-JL7lF7F7||FJL7LJFJ|||lL-JL---7F7L-7F--J|lF7|F--JL7FJ||||||L7FJ||||FJL-7l||FJ||L7|L7FJFJl|FJlL-7F7L7L7FJF-JFJ|FJllll.......
...............lL--7L7||||LJ|F7|lFJFJ|L----7lF-J|L7FJ|F-7|FJLJL--7FJL7||||||FJL7||LJ|F7FJFJ||FJL-J|FJL-JF-JL---7||L7L-JL-JF7|lLJlF7F7ll.....
..............lllllL7LJLJ|F7LJ||FJFJFJF7F-7L7L7FJFJ|lLJFJ||F7F7F-J|F-J||LJ||L7FJ|L7FJ||L7|FJ|L7F7l|L7F--JF-7F7FJ|L7|F-7F--JLJF-7FJLJL-7l....
.............lF--7F7L---7LJL7FJ|L7|lL7|LJlL7|FJ|FJFJF7FJFJ|||||L-7|L7lLJlFJ|FJ|FJFJL7||FJLJlL7||L-JFJ|F-7|l|||L7L7|LJFJL---7lL7||F--7FJl....
.............lL-7LJL----JF-7|L7L-JL-7|L-7F7LJL7|L7|FJLJFJFJ||||F7|L7L---7|FJL7|L7|F7|||L---7l||L7F7|lLJFJL7LJ|FJl|L-7L7F7F7L--JLJ|llLJl.....
..............llL-------7|FJL-JF7F7FJL-7LJL--7|L7LJL--7|FJFJ|||||L7|F---J||lFJL7|||LJ||F--7|FJ|FJ|||F--JF7L7FJL-7|F7|FJ||||F-7F-7L-7ll......
...............lF------7LJL7F7FJ|||L--7|F-7F-JL-JF----J|L7|FJ|LJ|FJ|L--7FJL7|F7|||L7FJ|L-7||L7|L7|LJL7F7|L-JL7F-JLJLJL7||||L7||FJF-Jl.......
..............llL-----7L---J|||FJ||F7FJ|L7|L--7lFJF-7F7L7||L7L7l|L7|F7FJL7FJ||LJLJFJL7|F7||L7||FJ|F--J||L---7|L---7F7FJ||||FJ||L7L7l........
.............lF7F7F7F7L----7||||FJ||LJlL7||F--JFJFJFJ|L7|||FJFJFJFJ|||L7FJ|l||lF--JF-J||LJ|FJ||L7|L-7FJ|F---JL7F-7LJ||FJ||LJlLJl|FJl........
.............l|LJLJLJL-----J|||||l|L--7FJ||L--7L-Jl|FJFJ|||L-JFJFJFJ|L-JL7L7||FJF7FJF7||F-J|FJL7LJF-J|FJL7F--7|L7|F-JLJl||ll.lF-JL7l........
.............lL-7F---------7||||L7|F--JL-JL-7FJF---JL7L-J||F--JFJlL7L---7L7|||L7|||l|LJ|L-7||F7|F7|F7|L-7LJF7||FJ|L----7|L7llFJF--Jl........
..............llLJlF7F-----J|||L-J|L------7FJ|FJF7F--JF--J||F7FJF7l|F7F7L7||LJl||LJFJF7L7l||LJ|||LJ|||F-JF-JLJ|L7|F7F7FJ|FJllL-Jlll.........
................lF-JLJF7F--7||L-7FJF7F7F--JL-JL7||L-7lL-7FJ|||L-J|FJ|||L7|LJF--JL7FJFJ|FJFJ|F7LJL-7|LJL-7L-7F-JFJLJLJ|L7||l..lll............
...............lFJF7F7|||F-J||F7||FJ|||L---7lF7LJ|F7L-7FJL7|||F--JL7|||FJ|F7|F-7FJL7L7||FJFJ||F7F-JL-7lFJF-JL-7L--7lFJFJ||l.................
...............lL7|LJ|||||F7||||||L7|||F-7FJFJL--J||F-JL7FJLJ||F--7LJ||L7LJ|||FJ|F-JFJ||L7|FJLJLJF-7FJFJFJlF-7L7F7L7|FJlLJl.................
................lLJF-J|||||||LJ|||FJ||||l|L7|F-7F7|||lF-JL-7FJLJF-JF7LJlL-7|LJ|FJ|F-Jl||FJ|L7F7F-JFJ|lL7|F7|FJl|||FJ||llll..................
.................llL-7||||||L7lLJ||FJ||L7L-J||FJ|||||FJF--7||F-7L--JL-7F--JL-7|L7||lll|||FJlLJ||F-JFJF-JLJLJL-7||LJl|L-7l...................
...................ll||LJ||L7L7F7LJL7|L7L-7FJ|L-J|||||FJF-J|||FJF-7F7FJL--7F7|L-JLJl.lLJ||F---J||F7L7L7F7F7F7FJ|L7llL7FJl...................
....................lLJlFJL7L7LJL--7LJlL7FJ|FJF--J||||L7L-7|||L7|l||LJllllLJLJlllll...llLJL---7||||FJl||||||LJl|FJl.lLJl....................
.....................lllL7FJFJF-7F7|ll.l||l|L7L---JLJL7L-7||||FJL7|L--7l..llll..........lllF--J||||L-7LJ|||L7ll||l...ll.....................
......................lF-JL7L7L7LJ|L-7ll||FJFJF-------JF-J|||||F7||F-7L7l.................lL-7FJ|||F-Jll|||FJllLJl..........................
......................lL7F7|lL7L7lL--Jll|||FJFJF7F7F7F7L-7|||||||||L7L-Jl..................ll||FJ||L-7llLJ||l..ll...........................
.......................l||LJllL7L-7lll.lLJLJFJFJ|||||||F7|LJLJLJLJL-Jlll...................lFJ||FJ|F7|l.llLJl...............................
.......................lLJll.lFJF-Jl....llllL-Jl||||||||||lllllllllll....................lll|FJ||lLJLJl...ll................................
........................ll...lL7L7l.........llllLJ||||||||l.............................lF--JL7LJlllll......................................
..............................lL7L7l............llLJLJ||LJl.............................lL----Jll...........................................
...............................lL-Jl..............llll||ll...............................llllll.............................................
................................lll..................lLJl...................................................................................
......................................................ll....................................................................................
............................................................................................................................................
............................................................................................................................................
............................................................................................................................................

OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOF7OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO||OOOF7OOOOOOOOOOOOOOOOOOOOF7OF7F7OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOFJL7OO||OOOOOOOOF7OOOOOOOOOO|L7||||OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOF7F7OOOOOOOOOOOOOOOOOOOOOOOOOOOF7OL7FJF7||F7OOOOOO||F7OOOOOOOOL7||LJ|OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO|LJ|F7F7OOOOOOOOOOOOOOOOF7OF7OFJ|F7||FJ||LJL-7OOOFJ|||OOOOOOOF7|||F7|OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOL-7LJLJL7OOOOOOF7OF7OOOFJL-JL7L7||||||FJL-7F-JOOOL7LJL7F--7OFJLJLJ|LJOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOL7F---JOOOOOO||FJ|F-7|F---7L-J||||LJL--7|L--7OOOL7F-J|F-JOL--7F-JF7OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOF-7F7|L--7OOOOOOFJ||FJL7|LJF--JF-7LJ||F----J|F--JF7F-J|.FJ|OOOF7FJ|..||OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOL7LJLJF--JF-7OOOL7|||F7||F7L7F-J.L-7LJL-7.F7||F--J|L-7|FJFJOF-J||FJ..|L7OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOL---7L-7.|FJOOOFJLJLJLJLJ|.LJF7.F-JF---JFJLJLJF7FJF7||L7|F7L-7||L7F7L7|OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOF7FJF-JFJL7OOOL-7F7F7F-7L7F-JL-JF7L7F7.L7F---JLJFJ|||FJLJL7FJLJFJ|L-JL7OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOFJ|L7L7FJF-JOOOF-J|LJ|L7L-JL---7FJL7LJ|F7|L-7F7F7L7||LJF---JL7F-J.L7F7FJOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO|FJ.|FJL7|.F7OOL--JF-JFJ.F7.F--J|F7L-7LJLJF-J|LJ|FJ||F-J.F7F7|L-7F-J|LJF7OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOF7FJL7FJL7FJL7|L-7F7F7L7FJF7|L7L---J||F-JF---JF-JF7||FJ|L--7|||||F-JL-7L--JL7OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOF-7||L7FJL-7||F7||F-J||||FJL-J||FJF----J|L-7|F7F7L--J||||FJF--J|LJLJL7F7FJF----JOOOOOF7OOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOL7LJL-J|F7FJLJ||LJL7FJ||LJF---J||.L7F7F7L7FJ||LJL--7.||||L7L7F7L-7F-7|||L7L----7F7OOO||OOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOL----7|||L-7FJL---JL7|L-7|F7..||F7LJLJL7|L7||F----JFJLJ|FJFJ|L-7||.||||FJF----J||F7FJL7OF7OOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOF---7|LJ|F7|L-7F7F7FJL7FJ||L7FJLJ|F7F7FJL-JLJL7F7F7L--7|L7|FJF7LJL7LJ||L7|.F7F7|||LJF-JO|L7OOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOL7F7LJF-J|||F-J||||L7FJL7||FJ|F--J||||L--7F7F7LJ|||F--JL-J||FJL-7FJF-J|.|L-J||LJ|L-7L---JFJOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOF7OOFJ|L-7L7FJ||L-7||||.||F-J||L7|L7F7|||L7F-J|LJ|F-J||L-----7LJL-7FJ|FJF7L7|F--J|F-JF-JF7F7FJOOF7OOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOF--7||F7L-JF7|FJL7LJF-J||||FJ|L-7||FJL7||||LJFJL--JF7||F7|L7.F7F-JF---JL-JL-JL7|||F7FJL7.L7FJLJLJF-7||OF-7OOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOF7OL-7|||||.F-JLJL--JF7|F7||||L7L7FJLJL7FJLJ|L-7|F7F-7|LJ||LJFJFJLJF7|.F7F7F7.F7FJLJ||LJF-JF7||F7F-7|FJ|L7L7|OOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOO||F7.|LJLJL7L--7F7F-7|LJ|||LJL7|FJL7F--JL7F7|F-JLJ|L7|L-7LJF7L7L---J|L-JLJ||L7||L-7FJ|F-JF7|||LJLJFJ||FJFJFJL7OOOOOOOOOOOO
OOOOOOOOOOOOOOOOOFJLJL-JF7F-7L7F7LJLJFJ|F-J||F--J|L7FJL7.F7||LJL--7FJFJL--JF-JL-J.F---JF7F--J|FJ|L7FJL-JL-7|||LJF--7L7||L7L7|F7|OOOOOOOOOOOO
OOOOOOOOOOOOOOOOFJF-----JLJ.|FJ||F7F7L7LJF7LJ|F--JFJL7FJFJ||L7.F7FJL7L-7F-7L7F---7L----J|L7F7|L7L7|L7F7F--J|||F-JF7L-J||.L7|||LJOOOOOOOOOOOO
OOOOOOOOOOOOOOOOL-J.F7F--7F-J|FJLJ|||.L7FJ|F-JL--7|F7||.|FJL7L7||L-7L--JL7L7||F--JF7F---JFJ||L7|FJL-J|LJF7.|LJ|F-JL--7||F7|LJL--7OOOOOOOOOOO
OOOOOOOOOOOOOOOOF7.FJ|L-7|L-7|L--7LJ|F7||.|L-7F-7|||LJL7||F7L7||L7.L---7FJ.|LJL-7.||L---7L-J|FJ|L---7L7.|L7L-7LJF----J|||||F----JOOOOOOOOOOO
OOOOOOOOOOOOOOOFJL7L7|F7||F-JL7F7L-7||LJL7L--J|FJ||L-7FJ||||.|||FJF7F7.LJF7|F---JFJ|F7F7|F--J|FJF--7L7L-JFJF7|F-J.F--7|LJ|||F7F7OOOOOOOOOOOO
OOOOOOOOOOOOOF-JF7L7||||||L-7FJ|L7FJ||F--JF7F7||FJL7FJL-JLJL7|LJL7|LJ|F7FJLJL---7|FJ|LJ|||.F7|L7|F-JFJF--JFJLJ|.F7|F7|L-7LJLJLJ|F7OOOOOOOOOO
OOOOOOOOOOOOO|F-JL7|||||||F7|L-JFJL7|||F-7|||LJ||F-JL-7F----JL-7FJL-7||LJF-7F7F7||L7L-7LJL7|||FJ|L-7L7L7F7|F--JFJ|||LJF7|F7F--7LJL7OOOOOOOOO
OOOOOOOOOOOOOLJF--JLJ||||||LJF-7L--J||||FJ||L-7|||F7.FJ|F7F7.F-JL7.FJ||F-J.||LJLJ|FJF7L7F-J|LJL7|F7|FJFJ||||.F-JFJ|L-7||LJ||F7L---JOOOOOOOOO
OOOOOOOOOOOOOOOL----7|||||L-7|.L---7||||L7|L-7|||LJL7L7||||L7L-7FJFJFJLJF7FJL---7||FJ|FJL-7L-7FJLJ||L7|FJLJL7|F-J.|F-J|L7.|LJL-7F--7OOOOOOOO
OOOOOOOOOOOOOOF-----JLJLJ|F7LJF7F7.||||L7||F-J||L--7L-JLJ||FJF-JL-JFJF7FJLJF----J||L7|L7F-JF7|L-7.||FJLJF---J||.F-JL-7L7|FJF7F-J|F-JOOOOOOOO
OOOOOOOOOOOOF7L--7F7F-7F7LJL--JLJ|FJLJL7|LJL7FJL-7.L-7F--J|L7L----7L7||L--7L-7F7FJL7||FJL-7|||F-JFJ|L-7FJF7F7|L-JF---JFJLJFJ|L--JL7OOOOOOOOO
OOOOOOOOOOOFJL--7LJLJ.LJL7F7F---7|L---7|L-7FJ|F--JF7.||.F7|FJF7.F7L7|||F7.|F-J||L7FJ||L7F-J||||F7L7|F7||FJLJLJF--JF7F7|F7FJ.L7F-7FJOOOOOOOOO
OOOOOOOOOOOL---7L7F-7F---J|LJF--JL7F-7|L7FJ|FJ|F7FJL7|L7|||L7|L7|L7|LJ|||FJ|F7||FJL7||FJ|F7|||||L7|LJ||||F---7L7F-J|||||LJF7.||.LJF7F7OOOOOO
OOOOOOOOOOOOOOOL7LJFJL----JF7L7F-7LJ.|L7||FJ|FJ|||F-JL7||LJ.||.||FJ|F-J|||.LJ||||F-J|||.||||||||.|L7FJ||||F7.L7LJF-J|LJL7FJL7|L7F7|LJ|OOOOOO
OOOOOOOOOOOOOOF7L-7|F7.F---JL-J|FJF-7|FJLJL-J|FJLJL7F7||L-7FJL7LJL7||F7||L-7FJ|LJL7FJLJFJ|||LJLJFJFJL7|LJLJL7.L--JF7|F-7LJF7||FJ|||F-JOOOOOO
OOOOOOOOOOOOF-JL--JLJ|FJF------JL7|FJLJF7.F--JL--7.||LJ|F7||F-JF--J|||LJ|F7|L7|F--JL--7L-J|L-7F-JFJ..LJF----JF7F--JLJL7L7FJLJ|L-JLJ|OOOOOOOO
OOOOOOOOOOOOL-------7|L-JF7F--7F-J|L---J|FJF-7F7FJFJL-7|||||L7FJF-7||L7FJ|LJ.LJL7F7F--JF--JF7||F7L-7F--JF----JLJF7F---J.LJF-7L--7F7L-7OOOOOO
OOOOOOOOOOOOF7F----7||.F7||L7FJL--JF-7F7||FJ.||LJFJF7FJ|||||FJL7|.LJL7|L7L---7F-J||L--7L--7|||LJL7FJ|F-7|F-----7||L-7F7F7.|FJF7.|||F7L--7OOO
OOOOOOOOOF--J|L---7LJL-JLJL7||F-7F-J.LJLJLJ.FJL-7|FJ|L7|||||L7FJL--7FJ|FJF7F-J|F7||F-7L7F-J||L-7FJ|.LJ.LJL7F7F7LJ|F7LJLJL-JL-JL7||LJ|F--JOOO
OOOOOOOOOL--7L----JF7F7F7F7LJ|L7|L---------7L7F-JLJ.L7||||||FJ|F7F-JL7||FJ|L7FJ|||||FJFJ|F7||F7|L7|F----7FJ|LJ|F7LJ|F-7F-------JLJOOLJOOOOOO
OOOOOOOOF---JF----7|LJLJLJ|F7L-JL----7F7F-7|FJL-----7|||||||L7||LJF7FJ||L7L-JL7||||||FJ.LJ||||||FJ|L---7|L-JF7LJL-7LJ.|L--7.F7.F7OOOOOOOOOOO
OOOOOOOOL-7F7|F---JL7F----J||F7F-7F-7|||L7LJL7F7F---JLJLJ||L7|||F-JLJFJ|FJF---J|||||||F---J|LJLJL-J.F--JL---J|F-7FJF-7L7F7L-JL7||OOOOOOOOOOO
OOOOOOOOOOLJLJL7F---JL7F-7FJLJLJ.LJFJLJL-J.F-J||L----7.F7LJ.LJLJL--7FJ.||.|F7F7|LJ||||L-7F7|F---7F-7L-------7LJ.LJ.|FJ.|||F7F7LJ|OOOOOOOOOOO
OOOOOOOOOOOF---J|F--7.LJ.LJF7F7F7F-JF7F---7L--J|F---7L-JL-7.F7F----J|F-JL7LJLJLJF-J|LJ.FJ||||F-7|L7L7F7F----JF7F---JL-7LJLJLJL--JOOOOOOOOOOO
OOOOOOOOOOOL-7F7|L-7|F7.F7FJLJLJ|L--JLJF--JF--7|L-7.|F7F-7|FJLJF-7F7|L7F7L-----7L--J.F7L-JLJLJFJL7L7|||L---7.||L7F7F--J.F7F7.F7F---7OOOOOOOO
OOOOOOOOOOF--J|||F-J|||FJLJF---7|F--7F7|F-7L7FJL-7L7|||L7||L7F7L7|||L7LJL7F-7F7L-7F--JL-7F7F7FJF7L-JLJ|F--7|FJ|FJ||L-7F7|LJL7|LJF7FJOOOOOOOO
OOOOOOOOOOL---JLJL-7|||L-7FJ.F-JLJF7LJLJ|FJFJL--7L7||||FJLJ.LJL7|||L-JF7FJ|.LJ|F-JL7F--7||LJLJFJL----7|L-7LJ|FJL7|L--J|LJF-7LJF-JLJOOOOOOOOO
OOOOOOOOOOF-7F7.F7FJLJL-7LJF7L-7F-JL--7FJL-JF---J.|||||L-7F7F--J|||.F-J|L7L--7|L7..LJF7||L---7|F7F---JL-7L--JL--JL7.F7|F-J.|F7L---7OOOOOOOOO
OOOOOOOOOOL7|||FJLJF-7F7L--JL--J|F---7LJF7F7L----7||LJL--J|LJF--JLJFJF7L-JF7FJL7L7F--JLJL----JLJ|L-----7L-7F7F7F-7L-JLJL--7LJL---7|OOOOOOOOO
OOOOOOOOOOO|LJ|L7F7L7LJL7F-7F7F7|L-7.L--JLJL7F---J|L7F7F7.L-7L7F---JFJ|F-7|||F-JFJ|F--7F-----7F7|F-----JF7||LJLJ.|F7F7F---JF7F7..LJOOOOOOOOO
OOOOOOOOOOFJF7L-J|L7|F--J|FJ|LJLJF-JF-----7.LJF--7L-J|||L-7.|FJ|F7F7|FJ|.||||L-7L7||F-J|F----J|LJL-----7||LJF7F-7LJLJLJ.F--JLJL--7OOOOOOOOOO
OOOOOOOOOOL-JL---J.LJL---JL7|F---JF7L7F---JF7.|F-JF7.|||F-JFJL7|||||||FJFJ||L7FJFJLJL7FJL7F7F7|F------7LJL-7|||FJ.F7F7F7|F-------JOF7OOOOOOO
OOOOOOOOOOOOOOF----------7.LJL7F--JL-JL----JL7|L--J|FJLJ|F7L--J||LJ||||.L7|L7|L7L-7F-JL7.LJ||||L7F-7F7L-7F7LJLJL--JLJLJLJL7F7F-7F7O||F7OOOOO
OOOOOOOOOOOOOOL7F7F7F--7FJF7F7LJF-7F7F7F---7FJ|F---JL7F-J||.F-7LJF-J|LJ..||.||FJF7||F7FJF--J|||FJL7LJ|F7LJL--7F7F-7F------J|||FJ||FJLJ|OOOOO
OOOOOOOOOOOOOF-J|||LJF-JL-JLJ|F7L7LJ|||L--7LJFJL7F7.FJL--JL-JFJF-JF7L7...LJ.||L7|LJLJ|L7|F-7|LJL7FJF-J|L-7.F7LJLJ.|L-------JLJL7|||F--JOOOOO
OOOOOOOOOOOOOL-7|LJ.FJF--7F-7LJL7L-7LJ|F-7L--JF7LJL-JF7F7F---J.L--JL7|......||.|L--7FJFJLJ.||F-7||.L--JF7L-JL-7F-7|F7F7F7F-----J||||F7OOOOOO
OOOOOOOOOF-7OOFJL7F-JFJF7LJ.L--7|F-JF7LJ.L--7FJL7F---JLJLJ.F7F7F7...||......LJFJF7FJL-J..F7LJL7LJ|F----JL-----J|FJLJLJLJ|L------JLJLJ|OOOOOO
OOOOOOOOOL7|OOL--J|F7|FJL-----7||L--JL7.F-7.|L-7|L---------JLJ|||F7.LJ........L7|||F7.F--JL7F7L-7|L--7F-----7F-JL-7F7F-7|F7F-7F-7F7F-JOOOOOO
OOOOOOOOOFJ|OF7.F7LJLJL------7LJL-----JFJFJFJF-J|F7F7F-7F7F7F7LJLJ|............|||LJL7L---7LJ|F7LJ.F7LJF----J|F---J|LJFJLJLJ.LJ.LJ|L7OOOOOOO
OOOOOOOOOL7L7||FJL7.F----7.F7L---------JFJFJFJ.FJ|LJLJ.||LJ||L7F-7|...........FJ|L-7FJ.F-7L-7LJL---JL-7|F7F--JL----JF7L7.F7.F----7L-JOOOOOOO
OOOOOOOOOO|FJ||L-7|FJF--7|FJL-----7F7F7FJ.L-JF7L7L7F7F7|L-7||FJL7LJ...........L-J..LJ..L7L-7|F-7F----7|LJLJF7F------JL-JFJL-JF---J.F7F7OOOOO
OOOOOOOOOFJL-J|F7|LJFJF-JLJF-7F7F7LJ||LJ.F7F-JL7L7LJLJ||F-JLJL-7L-7...................F-JF7LJ|FJ|F7.FJL----J|L-7F-7F----JF---J.F-7FJLJL7OOOO
OOOOOOOOOL--7FJ||L-7|FJF7F-J.LJLJL-7||F--JLJF-7L7L7F--JLJF7...FJF-J...................|F-JL7FJL-J|L-JF-7F--7|F7LJFJ|F----JF7.F7|FJL7F--JOOOO
OOOOOOOOOF--JL-JL--J||FJLJF7F7F7F7FJLJ|F7F-7L7|FJFJL----7|L7..L7L7..............F7....LJF7.LJF7.FJF--J.LJF-J||L7FJFJL----7|L-J||L7FJL----7OO
OOOOOOOOFJF7F-7F7F--JLJF--JLJLJLJ|L---J||L7L-JLJ.L7F----J|FJ..FJFJ..............||.....FJL---JL7L-JF7F7F7L7.LJ.LJ.|F-----J|F--J|FJL7F7F--JOO
OOOOOOOOL-JLJ.LJLJF7F-7L7F------7|.F---JL-JF--7.F7LJF7F7.|L7..L-J..............FJ|.F7F-JF---7F7L---JLJ||L-JF7F7F7FJL---7F7||F--JL-7||LJOOOOO
OOOOOOOOOOF-----7.||L7|FJ|F-----JL-JF----7.L-7L7||.FJLJL7|FJ...................|FJFJ|L-7|F--J|L7F7F--7|L---JLJLJLJF----J|||LJF-7F7LJL7OOOOOO
OOOOOOOOOOL----7L-JL-J|L7|L7F--7F7F-JF---JF-7L7||L7|F---J||.F7.................|L-JFJF7LJL---J.LJLJF7|L---------7FJF--7.|||F-J.LJL-7FJOOOOOO
OOOOOOOOOOF----JF----7|.||FJ|F-J|||F7|F7.FJFJFJ||FJ||F7F-JL-J|.................|F-7L-J|.F7F7F7F7F7FJLJF7F7F7F--7LJFJF7L7|LJL----7F7||OOOOOOO
OOOOOOOOOOL-7F7FJF---JL7LJL-JL--JLJ|LJ|L7L7L7L7||L-JLJLJF7F-7|.................LJ.L7F7L7|LJLJLJLJLJF--JLJLJ||F-JF-JFJL7LJF7F7F--J||LJOOOOOOO
OOOOOOOOOOOOLJLJ.L----7L----7F7.F7FJF-JFJ.|FJFJLJF------JLJ.||.....................LJL7LJF-------7FJF7F7.F-J||F7|F-J.FJF7|LJLJ.F7|L7OOOOOOOO
OOOOOOOOOOOOF------7F7L----7LJL-J|L-JF7L--JL-JF-7L-7F7F7F--7LJ......................F-JF7|F----7FJ|FJLJL7L--JLJLJL--7L-J|L--7F7|LJFJOOOOOOOO
OOOOOOOOOOOOL-7F7F7LJL----7L----7L---JL---7F7FJFJF7LJLJLJF7L7.......................L-7|LJL7F-7LJFJL7F-7L-7F7F7F7F--JF-7L7F7LJ||F7L-7OOOOOOO
OOOOOOOOOOOOF-J|LJL7F7F-7FJF7F-7L----7F--7|||L7L7|L7F-7F-JL-J..........F7..........F7FJ|F7.|L7L7FJF-J|FJF7LJ|||||L--7|FJ.LJ|F7|||L-7|OOOOOOO
OOOOOOOOOOOOL7FJF7.LJLJ.||FJLJFJF---7LJF-JLJL-J.LJ.|L7LJF---7F7........||........F-JLJFJ||FJFJ.LJ.L7FJL-JL7.LJ||L7F7LJL7.F-J|LJ|L-7LJOOOOOOO
OOOOOOOOOOOOOLJ.||F-----J|L--7L7L--7L-7L7.F7.F7F7F7L-J.FJF-7||L7.....F7|L7.......L-7F-JFJ|L-J.F--7.LJ.F---JF-7||.LJ|F-7L7L-7L--JF-JOOOOOOOOO
OOOOOF-7OOOOOF-7||L---7F7L---JFJF-7L-7L7L7||FJ||LJL7F--JFJ.LJL7L-7...||L7|F--7...F-JL7.L7|F7F7L-7L7F-7L----JFJLJF7.LJ.L-JF7L7F7FJOOOOOOOOOOO
OOOOO|FJF7OOOL7|||F7F7LJL--7F7L7L7|F7L7L-J|||FJ|F-7LJF-7L-7F7FJF-J..FJ|FJ||F-J...L-7FJF7|LJLJL-7L7|L7L7F---7L---JL-------JL7|||L-7OOOOOOOOOO
OOOOFJL-JL7OOFJLJLJLJ|.F7F7LJL7L-JLJL-J.F7|LJL7|L7L--J.|F7LJLJFJF7F7L7||FJ||.......LJFJLJF--7F7L-JL7L7LJF7.L7F---7F7F---7F-JLJ|F7|OOOOOOOOOO
OOOOL7F7F7L7FJF-7F--7|FJLJL7F7L---------JLJF-7LJFJF7.F7LJL----J.|||L7||||FJ|F7.....F7L---JF7LJL-7F7L-JF-JL7FJL-7.LJ|L--7|L7F7.LJLJF-7OOOOOOO
OOOOFJ|LJL7LJFJFJ|F-JLJF--7LJL---------7F7FJ.|F7L-JL-JL-----7F7FJ|L7||||LJFJ|L-7..FJL---7FJL7F-7|||F--JF7FJL7F7|F7.L-7FJL-J|L7F7.FJFJOOOOOOO
OOOOL-J.F-JF7L7L7|L7F7FJF-JF7F7F-----7.|||L7FJ||F-7F7F7F----J||L7|.||||L7FJ.|F7|F7L----7|L7FJL7LJ|LJF--JLJF7LJ|LJL--7|L7.F-JFJ|L7|FJOOOOOOOO
OOOOOOOFJF7|L7|.LJFJ|||FJF7|||||F---7L7LJ|FJL7|||.||LJLJ.F7F7||FJL-J|||FJL7FJ|LJ||..F7.|L-JL-7L--JF7L----7|L-7L-7F7FJL7L7|F-JFJFJ||F7OOOOOOO
OOOOOOOL-J|L7||F--JFJLJL-JLJLJLJL--7|FJF7LJ.FJ||L7|L-7F--JLJLJ||F-7FJ||L-7|L7L7FJL7FJL7L---7FJ.F7.|L-7F-7LJF-JF7LJ||F7L7LJL--JFJ.|LJL---7OOO
OOOOOOOOOO|FJ||L---J.F7.F7F-7F-7F--JLJFJL--7|FJ|FJ|F7|L7F7F7F7|LJFJL7||F7|L-JFJ|F7|L-7L-7F7||F-JL7L-7LJFJF7L7.|L-7|LJL7|F7F---J.FJF-7F7FJOOO
OOOOOOOOOOLJOLJF7F---JL7||L7|L7|L-----JF7F-JLJ.LJ.LJLJFJ|||LJLJF7L-7||LJ|L7F-J.||LJF-JF7LJLJ|L7F7L-7L-7L-JL7L-JF-JL-7FJLJ|L---7FJFJ.LJ||OOOO
OOOOOOOOOOOOOF-J|L----7||L-JL-JL--7F7F-J|L------7F7F7.L-J|L-7F-JL--JLJF7|FJL-7FJL7.L--JL---7L7LJL-7|F7L--7FJF--J.F-7LJF-7L----JL7L-7OOLJOOOO
OOOOOOOOOOOOOL-7|F7F--JLJF7F7F-7F7LJLJF7L----7F7||LJ|F7F-JF-JL------7FJLJL-7FJL7FJF7F7F7.F7L7L7F--JLJL---JL-JF7F7L7L7.|FJ.F7F---JF7L7OOOOOOO
OOOOOOOOOOOOOF-JLJLJF7F-7|||LJFJ||F7F7||F7F7.||LJ|F-J||L-7L-7F--7.F7|L7F7F7||..||.|LJLJ|FJ|.|FJL----------7F7|LJL-JFJFJL7FJ||F-7FJL-JOOOOOOO
OOOOOOOOOOOOOL------J||FJ|LJ.FJFJLJLJLJLJLJL7LJF7||.FJ|F7|F-J|F7L-J|L7LJ||||L7FJL7|F7F-JL7|FJ|.F7F--------J|||F----J.L7FJL7||L7|L--7OOOOOOOO
OOOOOOOOOOOOF7F7F7.F7LJL-JF-7L-JF----------7L7FJLJL7L7||LJL7.LJL--7L7|F-J|||FJ|F-JLJ|L-7.||L7|FJ|L--------7|LJL---7F7FJL7.|||FJL---JOOOOOOOO
OOOOOOOOOF--JLJLJL-JL-----JFJF7.L---------7|FJL---7L-J|L--7L-7.F7.L7||L-7|||L7||F7F-JF-JFJL-J||FJ.F7F7F---J|F-----J|LJF-JFJLJL-7F7.F7F7OOOOO
OOOOOOOOOL--7F-7F---7F7F--7L-JL7F----7F7F-J||F-7F7|F-7L-7.L-7|FJL--J||F-J||L7LJLJ|L-7L-7L7F-7|||F7||||L7F7.|L-7F7F7|F-JF-JF-7F7LJ|FJ|||OOOOO
OOOOOOOOOOOFJL7||F--J|LJF7L---7|L---7||LJF7LJL7LJ||L7L--JF7FJ|L-7F-7LJL--JL7L-7F-JF7L7FJ.LJFJ||LJ||||L7LJL-JF7LJLJLJL--JF-J.||L--J|FJ||OOOOO
OOOOOOOOOOOL7FJLJL---JF-JL----J|F7F-JLJF7|L--7|F-J|FJF7F7||L7|F7LJ.L--7F-7FJF-J|F-J|FJ|..F7L7LJF-J||L7L---7FJ|F--7F--7F7L--7LJF---JL-JL7OOOO
OOOOOOOOOOF7LJF7F----7L--7F7F-7|||L7F-7|||F--J|L7FJL7|||LJ|.||||F7F--7|L7LJ.L-7LJF-JL7L7FJL7L7FJ.FJ|FJF--7LJFJL-7|L-7||L7F7L--JF7F7F7F-JOOOO
OOOOOOOOOFJL7.||L---7L7F7||LJFJLJL-J|FJ||||F-7L7|L7FJ||L7FJFJLJLJ|L7FJ|FJ.F---JF-JF7.L7|L-7|.||F7|FJL-JF7L-7L---JL--JLJFJ|L7F--JLJ||LJOOOOOO
OOOOOOOOOL-7L-JL---7L7LJ|LJF7L7F----JL-JLJ||FJFJL7|L7||FJ|.L-7F--JFJL-J|F7L---7|F7||F7||F-JL7|||||L-7F7|L--JF--7F7F7F7.L7L7|L----7||OOOOOOOO
OOOOOOOOOOOL7F--7F7L-JF7L7FJL-J|.F7F7F7F--J|L7L-7LJFJ|||FJF7FJL7F7L-7F-J|L7F7.||||||||||L7F7||LJ||F-J||L--7.|F-J|||||L-7|FJL-----JLJOOOOOOOO
OOOOOOOOOOOO||F-J|L-7FJL7LJF7F7|FJLJLJLJ.F7|FJF-JF-J.||||.||L7FJ||F7|L-7|FJ||FJLJLJ|||||.LJ||L7FJ|L7FJL---JFJL--JLJLJF7||L-7F7..F7OOOOOOOOOO
OOOOOOOOOOOOLJ|F-JF-J|F7L-7|LJ||L-----7F7|||L7L-7|F-7||||FJL-J|FJ||LJF-J|L7||L-7F--J|||L7F7|L7|L-JFJL------JF7F------JLJ|F7LJL--JL7OOOOOOOOO
OOOOOOOOOOOOF-J|F-JF7||L--J|F-J|F-----J||||L7|F7||L7||||||F---JL7|L7FJF7L7|||F7||F7.|||FJ||L7|L7F-JF7F7F7F-7||L--------7LJL7F----7L7OOOOOOOO
OOOOOOOOOOOOL-7|L--JLJL7F-7|L--JL--7F7FJLJ|FJ||||L7|||||||L--7F7|L7||FJL7|||||||LJ|FJ|||FJL7||FJ|F7|LJ|||L7LJ|F--7F7F7FJ.F7|L---7|FJOOOOOOOO
OOOOOOOOOOOOOO||F--7F--J|FJ|F-7F---J||L--7LJ.LJLJFJ||||||L7F-J|LJ.||LJF-J|||LJ|L-7|L7||||F-J|||FJ|||F-J|L7L7.|L7.LJLJLJF-J|L--7FJ|L7OOOOOOOO
OOOOOOOOOOOOOOLJL-7|L---JL7|L7|L7F-7||F-7L7F-----JFJLJLJ|FJL-7L--7|L-7|F7|||F-JF7||FJ||LJL7FJ|||FJ||L-7L7L7L7L7L---7.F-JF-JF-7|L7L-JOOOOOOOO
OOOOOOOOOOOOOOOOF-JL7.F7F7||FJL7LJFJ|||.L-JL---7F7L-7F--J|.F7|F--JL7FJ||||||L7FJ||||FJL-7.||FJ||L7|L7FJFJ.|FJ.L-7F7L7L7FJF-JFJ|FJOOOOOOOOOOO
OOOOOOOOOOOOOOOOL--7L7||||LJ|F7|.FJFJ|L----7.F-J|L7FJ|F-7|FJLJL--7FJL7||||||FJL7||LJ|F7FJFJ||FJL-J|FJL-JF-JL---7||L7L-JL-JF7|.LJ.F7F7OOOOOOO
OOOOOOOOOOOOOOOOOOOL7LJLJ|F7LJ||FJFJFJF7F-7L7L7FJFJ|.LJFJ||F7F7F-J|F-J||LJ||L7FJ|L7FJ||L7|FJ|L7F7.|L7F--JF-7F7FJ|L7|F-7F--JLJF-7FJLJL-7OOOOO
OOOOOOOOOOOOOOF--7F7L---7LJL7FJ|L7|.L7|LJ.L7|FJ|FJFJF7FJFJ|||||L-7|L7.LJ.FJ|FJ|FJFJL7||FJLJ.L7||L-JFJ|F-7|.|||L7L7|LJFJL---7.L7||F--7FJOOOOO
OOOOOOOOOOOOOOL-7LJL----JF-7|L7L-JL-7|L-7F7LJL7|L7|FJLJFJFJ||||F7|L7L---7|FJL7|L7|F7|||L---7.||L7F7|.LJFJL7LJ|FJ.|L-7L7F7F7L--JLJ|..LJOOOOOO
OOOOOOOOOOOOOOOOL-------7|FJL-JF7F7FJL-7LJL--7|L7LJL--7|FJFJ|||||L7|F---J||.FJL7|||LJ||F--7|FJ|FJ|||F--JF7L7FJL-7|F7|FJ||||F-7F-7L-7OOOOOOOO
OOOOOOOOOOOOOOOOF------7LJL7F7FJ|||L--7|F-7F-JL-JF----J|L7|FJ|LJ|FJ|L--7FJL7|F7|||L7FJ|L-7||L7|L7|LJL7F7|L-JL7F-JLJLJL7||||L7||FJF-JOOOOOOOO
OOOOOOOOOOOOOOOOL-----7L---J|||FJ||F7FJ|L7|L--7.FJF-7F7L7||L7L7.|L7|F7FJL7FJ||LJLJFJL7|F7||L7||FJ|F--J||L---7|L---7F7FJ||||FJ||L7L7OOOOOOOOO
OOOOOOOOOOOOOOF7F7F7F7L----7||||FJ||LJ.L7||F--JFJFJFJ|L7|||FJFJFJFJ|||L7FJ|.||.F--JF-J||LJ|FJ||L7|L-7FJ|F---JL7F-7LJ||FJ||LJOLJ.|FJOOOOOOOOO
OOOOOOOOOOOOOO|LJLJLJL-----J|||||.|L--7FJ||L--7L-J.|FJFJ|||L-JFJFJFJ|L-JL7L7||FJF7FJF7||F-J|FJL7LJF-J|FJL7F--7|L7|F-JLJ.||OOOOF-JL7OOOOOOOOO
OOOOOOOOOOOOOOL-7F---------7||||L7|F--JL-JL-7FJF---JL7L-J||F--JFJ.L7L---7L7|||L7|||.|LJ|L-7||F7|F7|F7|L-7LJF7||FJ|L----7|L7OOFJF--JOOOOOOOOO
OOOOOOOOOOOOOOOOLJ.F7F-----J|||L-J|L------7FJ|FJF7F--JF--J||F7FJF7.|F7F7L7||LJ.||LJFJF7L7.||LJ|||LJ|||F-JF-JLJ|L7|F7F7FJ|FJOOL-JOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOF-JLJF7F--7||L-7FJF7F7F--JL-JL7||L-7.L-7FJ|||L-J|FJ|||L7|LJF--JL7FJFJ|FJFJ|F7LJL-7|LJL-7L-7F-JFJLJLJ|L7||OOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOFJF7F7|||F-J||F7||FJ|||L---7.F7LJ|F7L-7FJL7|||F--JL7|||FJ|F7|F-7FJL7L7||FJFJ||F7F-JL-7.FJF-JL-7L--7.FJFJ||OOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOL7|LJ|||||F7||||||L7|||F-7FJFJL--J||F-JL7FJLJ||F--7LJ||L7LJ|||FJ|F-JFJ||L7|FJLJLJF-7FJFJFJ.F-7L7F7L7|FJOLJOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOLJF-J|||||||LJ|||FJ||||.|L7|F-7F7|||.F-JL-7FJLJF-JF7LJ.L-7|LJ|FJ|F-JO||FJ|L7F7F-JFJ|.L7|F7|FJ.|||FJ||OOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOL-7||||||L7.LJ||FJ||L7L-J||FJ|||||FJF--7||F-7L--JL-7F--JL-7|L7||OOO|||FJ.LJ||F-JFJF-JLJLJL-7||LJO|L-7OOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOO||LJ||L7L7F7LJL7|L7L-7FJ|L-J|||||FJF-J|||FJF-7F7FJL--7F7|L-JLJOOOLJ||F---J||F7L7L7F7F7F7FJ|L7OOL7FJOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOLJOFJL7L7LJL--7LJOL7FJ|FJF--J||||L7L-7|||L7|.||LJOOOOLJLJOOOOOOOOOOLJL---7||||FJ.||||||LJO|FJOOOLJOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOL7FJFJF-7F7|OOOO||.|L7L---JLJL7L-7||||FJL7|L--7OOOOOOOOOOOOOOOOOOOOF--J||||L-7LJ|||L7OO||OOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOF-JL7L7L7LJ|L-7OO||FJFJF-------JF-J|||||F7||F-7L7OOOOOOOOOOOOOOOOOOOL-7FJ|||F-JOO|||FJOOLJOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOL7F7|OL7L7.L--JOO|||FJFJF7F7F7F7L-7|||||||||L7L-JOOOOOOOOOOOOOOOOOOOOO||FJ||L-7OOLJ||OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOO||LJOOL7L-7OOOOOLJLJFJFJ|||||||F7|LJLJLJLJL-JOOOOOOOOOOOOOOOOOOOOOOOFJ||FJ|F7|OOOOLJOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOLJOOOOFJF-JOOOOOOOOOL-JO||||||||||OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO|FJ||OLJLJOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOL7L7OOOOOOOOOOOOOOLJ||||||||OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOF--JL7LJOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOL7L7OOOOOOOOOOOOOOOLJLJ||LJOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOL----JOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOL-JOOOOOOOOOOOOOOOOOOO||OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOLJOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOF7OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO||OOOF7OOOOOOOOOOOOOOOOOOOOF7OF7F7OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOFJL7OO||OOOOOOOOF7OOOOOOOOOO|L7||||OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOF7F7OOOOOOOOOOOOOOOOOOOOOOOOOOOF7OL7FJF7||F7OOOOOO||F7OOOOOOOOL7||LJ|OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO|LJ|F7F7OOOOOOOOOOOOOOOOF7OF7OFJ|F7||FJ||LJL-7OOOFJ|||OOOOOOOF7|||F7|OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOL-7LJLJL7OOOOOOF7OF7OOOFJL-JL7L7||||||FJL-7F-JOOOL7LJL7F--7OFJLJLJ|LJOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOL7F---JOOOOOO||FJ|F-7|F---7L-J||||LJL--7|L--7OOOL7F-J|F-JOL--7F-JF7OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOF-7F7|L--7OOOOOOFJ||FJL7|LJF--JF-7LJ||F----J|F--JF7F-J|OFJ|OOOF7FJ|OO||OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOL7LJLJF--JF-7OOOL7|||F7||F7L7F-JOL-7LJL-7OF7||F--J|L-7|FJFJOF-J||FJOO|L7OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOL---7L-7O|FJOOOFJLJLJLJLJ|OLJF7OF-JF---JFJLJLJF7FJF7||L7|F7L-7||L7F7L7|OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOF7FJF-JFJL7OOOL-7F7F7F-7L7F-JL-JF7L7F7OL7F---JLJFJ|||FJLJL7FJLJFJ|L-JL7OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOFJ|L7L7FJF-JOOOF-J|LJ|L7L-JL---7FJL7LJ|F7|L-7F7F7L7||LJF---JL7F-JOL7F7FJOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO|FJO|FJL7|OF7OOL--JF-JFJOF7OF--J|F7L-7LJLJF-J|LJ|FJ||F-JOF7F7|L-7F-J|LJF7OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOF7FJL7FJL7FJL7|L-7F7F7L7FJF7|L7L---J||F-JF---JF-JF7||FJ|L--7|||||F-JL-7L--JL7OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOF-7||L7FJL-7||F7||F-J||||FJL-J||FJF----J|L-7|F7F7L--J||||FJF--J|LJLJL7F7FJF----JOOOOOF7OOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOL7LJL-J|F7FJLJ||LJL7FJ||LJF---J||OL7F7F7L7FJ||LJL--7O||||L7L7F7L-7F-7|||L7L----7F7OOO||OOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOL----7|||L-7FJL---JL7|L-7|F7OO||F7LJLJL7|L7||F----JFJLJ|FJFJ|L-7||O||||FJF----J||F7FJL7OF7OOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOF---7|LJ|F7|L-7F7F7FJL7FJ||L7FJLJ|F7F7FJL-JLJL7F7F7L--7|L7|FJF7LJL7LJ||L7|OF7F7|||LJF-JO|L7OOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOL7F7LJF-J|||F-J||||L7FJL7||FJ|F--J||||L--7F7F7LJ|||F--JL-J||FJL-7FJF-J|O|L-J||LJ|L-7L---JFJOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOF7OOFJ|L-7L7FJ||L-7||||O||F-J||L7|L7F7|||L7F-J|LJ|F-J||L-----7LJL-7FJ|FJF7L7|F--J|F-JF-JF7F7FJOOF7OOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOF--7||F7L-JF7|FJL7LJF-J||||FJ|L-7||FJL7||||LJFJL--JF7||F7|L7OF7F-JF---JL-JL-JL7|||F7FJL7OL7FJLJLJF-7||OF-7OOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOF7OL-7|||||OF-JLJL--JF7|F7||||L7L7FJLJL7FJLJ|L-7|F7F-7|LJ||LJFJFJLJF7|OF7F7F7OF7FJLJ||LJF-JF7||F7F-7|FJ|L7L7|OOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOO||F7O|LJLJL7L--7F7F-7|LJ|||LJL7|FJL7F--JL7F7|F-JLJ|L7|L-7LJF7L7L---J|L-JLJ||L7||L-7FJ|F-JF7|||LJLJFJ||FJFJFJL7OOOOOOOOOOOO
OOOOOOOOOOOOOOOOOFJLJL-JF7F-7L7F7LJLJFJ|F-J||F--J|L7FJL7OF7||LJL--7FJFJL--JF-JL-JOF---JF7F--J|FJ|L7FJL-JL-7|||LJF--7L7||L7L7|F7|OOOOOOOOOOOO
OOOOOOOOOOOOOOOOFJF-----JLJO|FJ||F7F7L7LJF7LJ|F--JFJL7FJFJ||L7OF7FJL7L-7F-7L7F---7L----J|L7F7|L7L7|L7F7F--J|||F-JF7L-J||OL7|||LJOOOOOOOOOOOO
OOOOOOOOOOOOOOOOL-JOF7F--7F-J|FJLJ|||OL7FJ|F-JL--7|F7||O|FJL7L7||L-7L--JL7L7||F--JF7F---JFJ||L7|FJL-J|LJF7O|LJ|F-JL--7||F7|LJL--7OOOOOOOOOOO
OOOOOOOOOOOOOOOOF7OFJ|L-7|L-7|L--7LJ|F7||O|L-7F-7|||LJL7||F7L7||L7OL---7FJO|LJL-7O||L---7L-J|FJ|L---7L7O|L7L-7LJF----J|||||F----JOOOOOOOOOOO
OOOOOOOOOOOOOOOFJL7L7|F7||F-JL7F7L-7||LJL7L--J|FJ||L-7FJ||||O|||FJF7F7OLJF7|F---JFJ|F7F7|F--J|FJF--7L7L-JFJF7|F-JOF--7|LJ|||F7F7OOOOOOOOOOOO
OOOOOOOOOOOOOF-JF7L7||||||L-7FJ|L7FJ||F--JF7F7||FJL7FJL-JLJL7|LJL7|LJ|F7FJLJL---7|FJ|LJ|||OF7|L7|F-JFJF--JFJLJ|OF7|F7|L-7LJLJLJ|F7OOOOOOOOOO
OOOOOOOOOOOOO|F-JL7|||||||F7|L-JFJL7|||F-7|||LJ||F-JL-7F----JL-7FJL-7||LJF-7F7F7||L7L-7LJL7|||FJ|L-7L7L7F7|F--JFJ|||LJF7|F7F--7LJL7OOOOOOOOO
OOOOOOOOOOOOOLJF--JLJ||||||LJF-7L--J||||FJ||L-7|||F7OFJ|F7F7OF-JL7OFJ||F-JO||LJLJ|FJF7L7F-J|LJL7|F7|FJFJ||||OF-JFJ|L-7||LJ||F7L---JOOOOOOOOO
OOOOOOOOOOOOOOOL----7|||||L-7|OL---7||||L7|L-7|||LJL7L7||||L7L-7FJFJFJLJF7FJL---7||FJ|FJL-7L-7FJLJ||L7|FJLJL7|F-JO|F-J|L7O|LJL-7F--7OOOOOOOO
OOOOOOOOOOOOOOF-----JLJLJ|F7LJF7F7O||||L7||F-J||L--7L-JLJ||FJF-JL-JFJF7FJLJF----J||L7|L7F-JF7|L-7O||FJLJF---J||OF-JL-7L7|FJF7F-J|F-JOOOOOOOO
OOOOOOOOOOOOF7L--7F7F-7F7LJL--JLJ|FJLJL7|LJL7FJL-7OL-7F--J|L7L----7L7||L--7L-7F7FJL7||FJL-7|||F-JFJ|L-7FJF7F7|L-JF---JFJLJFJ|L--JL7OOOOOOOOO
OOOOOOOOOOOFJL--7LJLJOLJL7F7F---7|L---7|L-7FJ|F--JF7O||OF7|FJF7OF7L7|||F7O|F-J||L7FJ||L7F-J||||F7L7|F7||FJLJLJF--JF7F7|F7FJOL7F-7FJOOOOOOOOO
OOOOOOOOOOOL---7L7F-7F---J|LJF--JL7F-7|L7FJ|FJ|F7FJL7|L7|||L7|L7|L7|LJ|||FJ|F7||FJL7||FJ|F7|||||L7|LJ||||F---7L7F-J|||||LJF7O||OLJF7F7OOOOOO
OOOOOOOOOOOOOOOL7LJFJL----JF7L7F-7LJI|L7||FJ|FJ|||F-JL7||LJI||I||FJ|F-J|||ILJ||||F-J|||I||||||||I|L7FJ||||F7OL7LJF-J|LJL7FJL7|L7F7|LJ|OOOOOO
OOOOOOOOOOOOOOF7L-7|F7OF---JL-J|FJF-7|FJLJL-J|FJLJL7F7||L-7FJL7LJL7||F7||L-7FJ|LJL7FJLJFJ|||LJLJFJFJL7|LJLJL7OL--JF7|F-7LJF7||FJ|||F-JOOOOOO
OOOOOOOOOOOOF-JL--JLJ|FJF------JL7|FJLJF7IF--JL--7I||LJ|F7||F-JF--J|||LJ|F7|L7|F--JL--7L-J|L-7F-JFJIILJF----JF7F--JLJL7L7FJLJ|L-JLJ|OOOOOOOO
OOOOOOOOOOOOL-------7|L-JF7F--7F-J|L---J|FJF-7F7FJFJL-7|||||L7FJF-7||L7FJ|LJILJL7F7F--JF--JF7||F7L-7F--JF----JLJF7F---JOLJF-7L--7F7L-7OOOOOO
OOOOOOOOOOOOF7F----7||OF7||L7FJL--JF-7F7||FJI||LJFJF7FJ|||||FJL7|ILJL7|L7L---7F-J||L--7L--7|||LJL7FJ|F-7|F-----7||L-7F7F7O|FJF7O|||F7L--7OOO
OOOOOOOOOF--J|L---7LJL-JLJL7||F-7F-JILJLJLJIFJL-7|FJ|L7|||||L7FJL--7FJ|FJF7F-J|F7||F-7L7F-J||L-7FJ|ILJILJL7F7F7LJ|F7LJLJL-JL-JL7||LJ|F--JOOO
OOOOOOOOOL--7L----JF7F7F7F7LJ|L7|L---------7L7F-JLJIL7||||||FJ|F7F-JL7||FJ|L7FJ|||||FJFJ|F7||F7|L7|F----7FJ|LJ|F7LJ|F-7F-------JLJOOLJOOOOOO
OOOOOOOOF---JF----7|LJLJLJ|F7L-JL----7F7F-7|FJL-----7|||||||L7||LJF7FJ||L7L-JL7||||||FJILJ||||||FJ|L---7|L-JF7LJL-7LJO|L--7OF7OF7OOOOOOOOOOO
OOOOOOOOL-7F7|F---JL7F----J||F7F-7F-7|||L7LJL7F7F---JLJLJ||L7|||F-JLJFJ|FJF---J|||||||F---J|LJLJL-JIF--JL---J|F-7FJF-7L7F7L-JL7||OOOOOOOOOOO
OOOOOOOOOOLJLJL7F---JL7F-7FJLJLJOLJFJLJL-JIF-J||L----7IF7LJILJLJL--7FJI||I|F7F7|LJ||||L-7F7|F---7F-7L-------7LJOLJO|FJO|||F7F7LJ|OOOOOOOOOOO
OOOOOOOOOOOF---J|F--7OLJOLJF7F7F7F-JF7F---7L--J|F---7L-JL-7IF7F----J|F-JL7LJLJLJF-J|LJIFJ||||F-7|L7L7F7F----JF7F---JL-7LJLJLJL--JOOOOOOOOOOO
OOOOOOOOOOOL-7F7|L-7|F7OF7FJLJLJ|L--JLJF--JF--7|L-7I|F7F-7|FJLJF-7F7|L7F7L-----7L--JIF7L-JLJLJFJL7L7|||L---7O||L7F7F--JOF7F7OF7F---7OOOOOOOO
OOOOOOOOOOF--J|||F-J|||FJLJF---7|F--7F7|F-7L7FJL-7L7|||L7||L7F7L7|||L7LJL7F-7F7L-7F--JL-7F7F7FJF7L-JLJ|F--7|FJ|FJ||L-7F7|LJL7|LJF7FJOOOOOOOO
OOOOOOOOOOL---JLJL-7|||L-7FJOF-JLJF7LJLJ|FJFJL--7L7||||FJLJILJL7|||L-JF7FJ|ILJ|F-JL7F--7||LJLJFJL----7|L-7LJ|FJL7|L--J|LJF-7LJF-JLJOOOOOOOOO
OOOOOOOOOOF-7F7OF7FJLJL-7LJF7L-7F-JL--7FJL-JF---JI|||||L-7F7F--J|||IF-J|L7L--7|L7IILJF7||L---7|F7F---JL-7L--JL--JL7OF7|F-JO|F7L---7OOOOOOOOO
OOOOOOOOOOL7|||FJLJF-7F7L--JL--J|F---7LJF7F7L----7||LJL--J|LJF--JLJFJF7L-JF7FJL7L7F--JLJL----JLJ|L-----7L-7F7F7F-7L-JLJL--7LJL---7|OOOOOOOOO
OOOOOOOOOOO|LJ|L7F7L7LJL7F-7F7F7|L-7IL--JLJL7F---J|L7F7F7IL-7L7F---JFJ|F-7|||F-JFJ|F--7F-----7F7|F-----JF7||LJLJO|F7F7F---JF7F7OOLJOOOOOOOOO
OOOOOOOOOOFJF7L-J|L7|F--J|FJ|LJLJF-JF-----7ILJF--7L-J|||L-7I|FJ|F7F7|FJ|I||||L-7L7||F-J|F----J|LJL-----7||LJF7F-7LJLJLJOF--JLJL--7OOOOOOOOOO
OOOOOOOOOOL-JL---JOLJL---JL7|F---JF7L7F---JF7I|F-JF7I|||F-JFJL7|||||||FJFJ||L7FJFJLJL7FJL7F7F7|F------7LJL-7|||FJOF7F7F7|F-------JOF7OOOOOOO
OOOOOOOOOOOOOOF----------7OLJL7F--JL-JL----JL7|L--J|FJLJ|F7L--J||LJ||||IL7|L7|L7L-7F-JL7ILJ||||L7F-7F7L-7F7LJLJL--JLJLJLJL7F7F-7F7O||F7OOOOO
OOOOOOOOOOOOOOL7F7F7F--7FJF7F7LJF-7F7F7F---7FJ|F---JL7F-J||IF-7LJF-J|LJII||I||FJF7||F7FJF--J|||FJL7LJ|F7LJL--7F7F-7F------J|||FJ||FJLJ|OOOOO
OOOOOOOOOOOOOF-J|||LJF-JL-JLJ|F7L7LJ|||L--7LJFJL7F7IFJL--JL-JFJF-JF7L7IIILJI||L7|LJLJ|L7|F-7|LJL7FJF-J|L-7OF7LJLJO|L-------JLJL7|||F--JOOOOO
OOOOOOOOOOOOOL-7|LJOFJF--7F-7LJL7L-7LJ|F-7L--JF7LJL-JF7F7F---JIL--JL7|IIIIII||I|L--7FJFJLJI||F-7||IL--JF7L-JL-7F-7|F7F7F7F-----J||||F7OOOOOO
OOOOOOOOOF-7OOFJL7F-JFJF7LJOL--7|F-JF7LJIL--7FJL7F---JLJLJIF7F7F7III||IIIIIILJFJF7FJL-JIIF7LJL7LJ|F----JL-----J|FJLJLJLJ|L------JLJLJ|OOOOOO
OOOOOOOOOL7|OOL--J|F7|FJL-----7||L--JL7IF-7I|L-7|L---------JLJ|||F7ILJIIIIIIIIL7|||F7IF--JL7F7L-7|L--7F-----7F-JL-7F7F-7|F7F-7F-7F7F-JOOOOOO
OOOOOOOOOFJ|OF7OF7LJLJL------7LJL-----JFJFJFJF-J|F7F7F-7F7F7F7LJLJ|IIIIIIIIIIII|||LJL7L---7LJ|F7LJIF7LJF----J|F---J|LJFJLJLJOLJOLJ|L7OOOOOOO
OOOOOOOOOL7L7||FJL7OF----7OF7L---------JFJFJFJIFJ|LJLJI||LJ||L7F-7|IIIIIIIIIIIFJ|L-7FJIF-7L-7LJL---JL-7|F7F--JL----JF7L7OF7OF----7L-JOOOOOOO
OOOOOOOOOO|FJ||L-7|FJF--7|FJL-----7F7F7FJIL-JF7L7L7F7F7|L-7||FJL7LJIIIIIIIIIIIL-JIILJIIL7L-7|F-7F----7|LJLJF7F------JL-JFJL-JF---JOF7F7OOOOO
OOOOOOOOOFJL-J|F7|LJFJF-JLJF-7F7F7LJ||LJIF7F-JL7L7LJLJ||F-JLJL-7L-7IIIIIIIIIIIIIIIIIIIF-JF7LJ|FJ|F7IFJL----J|L-7F-7F----JF---JOF-7FJLJL7OOOO
OOOOOOOOOL--7FJ||L-7|FJF7F-JOLJLJL-7||F--JLJF-7L7L7F--JLJF7IIIFJF-JIIIIIIIIIIIIIIIIIII|F-JL7FJL-J|L-JF-7F--7|F7LJFJ|F----JF7OF7|FJL7F--JOOOO
OOOOOOOOOF--JL-JL--J||FJLJF7F7F7F7FJLJ|F7F-7L7|FJFJL----7|L7IIL7L7IIIIIIIIIIIIIIF7IIIILJF7ILJF7IFJF--JILJF-J||L7FJFJL----7|L-J||L7FJL----7OO
OOOOOOOOFJF7F-7F7F--JLJF--JLJLJLJ|L---J||L7L-JLJIL7F----J|FJIIFJFJIIIIIIIIIIIIII||IIIIIFJL---JL7L-JF7F7F7L7OLJOLJO|F-----J|F--J|FJL7F7F--JOO
OOOOOOOOL-JLJOLJLJF7F-7L7F------7|OF---JL-JF--7IF7LJF7F7I|L7IIL-JIIIIIIIIIIIIIIFJ|IF7F-JF---7F7L---JLJ||L-JF7F7F7FJL---7F7||F--JL-7||LJOOOOO
OOOOOOOOOOF-----7O||L7|FJ|F-----JL-JF----7IL-7L7||IFJLJL7|FJIIIIIIIIIIIIIIIIIII|FJFJ|L-7|F--J|L7F7F--7|L---JLJLJLJF----J|||LJF-7F7LJL7OOOOOO
OOOOOOOOOOL----7L-JL-J|L7|L7F--7F7F-JF---JF-7L7||L7|F---J||IF7IIIIIIIIIIIIIIIII|L-JFJF7LJL---JILJLJF7|L---------7FJF--7O|||F-JOLJL-7FJOOOOOO
OOOOOOOOOOF----JF----7|O||FJ|F-J|||F7|F7IFJFJFJ||FJ||F7F-JL-J|IIIIIIIIIIIIIIIII|F-7L-J|IF7F7F7F7F7FJLJF7F7F7F--7LJFJF7L7|LJL----7F7||OOOOOOO
OOOOOOOOOOL-7F7FJF---JL7LJL-JL--JLJ|LJ|L7L7L7L7||L-JLJLJF7F-7|IIIIIIIIIIIIIIIIILJIL7F7L7|LJLJLJLJLJF--JLJLJ||F-JF-JFJL7LJF7F7F--J||LJOOOOOOO
OOOOOOOOOOOOLJLJOL----7L----7F7OF7FJF-JFJI|FJFJLJF------JLJI||IIIIIIIIIIIIIIIIIIIIILJL7LJF-------7FJF7F7IF-J||F7|F-JOFJF7|LJLJOF7|L7OOOOOOOO
OOOOOOOOOOOOF------7F7L----7LJL-J|L-JF7L--JL-JF-7L-7F7F7F--7LJIIIIIIIIIIIIIIIIIIIIIIF-JF7|F----7FJ|FJLJL7L--JLJLJL--7L-J|L--7F7|LJFJOOOOOOOO
OOOOOOOOOOOOL-7F7F7LJL----7L----7L---JL---7F7FJFJF7LJLJLJF7L7IIIIIIIIIIIIIIIIIIIIIIIL-7|LJL7F-7LJFJL7F-7L-7F7F7F7F--JF-7L7F7LJ||F7L-7OOOOOOO
OOOOOOOOOOOOF-J|LJL7F7F-7FJF7F-7L----7F--7|||L7L7|L7F-7F-JL-JIIIIIIIIIIF7IIIIIIIIIIF7FJ|F7I|L7L7FJF-J|FJF7LJ|||||L--7|FJOLJ|F7|||L-7|OOOOOOO
OOOOOOOOOOOOL7FJF7OLJLJO||FJLJFJF---7LJF-JLJL-JILJI|L7LJF---7F7IIIIIIII||IIIIIIIIF-JLJFJ||FJFJILJIL7FJL-JL7OLJ||L7F7LJL7OF-J|LJ|L-7LJOOOOOOO
OOOOOOOOOOOOOLJO||F-----J|L--7L7L--7L-7L7IF7IF7F7F7L-JIFJF-7||L7IIIIIF7|L7IIIIIIIL-7F-JFJ|L-JIF--7ILJIF---JF-7||OLJ|F-7L7L-7L--JF-JOOOOOOOOO
OOOOOF-7OOOOOF-7||L---7F7L---JFJF-7L-7L7L7||FJ||LJL7F--JFJILJL7L-7III||L7|F--7IIIF-JL7IL7|F7F7L-7L7F-7L----JFJLJF7OLJOL-JF7L7F7FJOOOOOOOOOOO
OOOOO|FJF7OOOL7|||F7F7LJL--7F7L7L7|F7L7L-J|||FJ|F-7LJF-7L-7F7FJF-JIIFJ|FJ||F-JIIIL-7FJF7|LJLJL-7L7|L7L7F---7L---JL-------JL7|||L-7OOOOOOOOOO
OOOOFJL-JL7OOFJLJLJLJ|OF7F7LJL7L-JLJL-JIF7|LJL7|L7L--JI|F7LJLJFJF7F7L7||FJ||IIIIIIILJFJLJF--7F7L-JL7L7LJF7OL7F---7F7F---7F-JLJ|F7|OOOOOOOOOO
OOOOL7F7F7L7FJF-7F--7|FJLJL7F7L---------JLJF-7LJFJF7IF7LJL----JI|||L7||||FJ|F7IIIIIF7L---JF7LJL-7F7L-JF-JL7FJL-7OLJ|L--7|L7F7OLJLJF-7OOOOOOO
OOOOFJ|LJL7LJFJFJ|F-JLJF--7LJL---------7F7FJI|F7L-JL-JL-----7F7FJ|L7||||LJFJ|L-7IIFJL---7FJL7F-7|||F--JF7FJL7F7|F7OL-7FJL-J|L7F7OFJFJOOOOOOO
OOOOL-JOF-JF7L7L7|L7F7FJF-JF7F7F-----7I|||L7FJ||F-7F7F7F----J||L7|I||||L7FJI|F7|F7L----7|L7FJL7LJ|LJF--JLJF7LJ|LJL--7|L7OF-JFJ|L7|FJOOOOOOOO
OOOOOOOFJF7|L7|OLJFJ|||FJF7|||||F---7L7LJ|FJL7|||I||LJLJIF7F7||FJL-J|||FJL7FJ|LJ||IIF7I|L-JL-7L--JF7L----7|L-7L-7F7FJL7L7|F-JFJFJ||F7OOOOOOO
OOOOOOOL-J|L7||F--JFJLJL-JLJLJLJL--7|FJF7LJIFJ||L7|L-7F--JLJLJ||F-7FJ||L-7|L7L7FJL7FJL7L---7FJIF7I|L-7F-7LJF-JF7LJ||F7L7LJL--JFJO|LJL---7OOO
OOOOOOOOOO|FJ||L---JOF7OF7F-7F-7F--JLJFJL--7|FJ|FJ|F7|L7F7F7F7|LJFJL7||F7|L-JFJ|F7|L-7L-7F7||F-JL7L-7LJFJF7L7O|L-7|LJL7|F7F---JOFJF-7F7FJOOO
OOOOOOOOOOLJOLJF7F---JL7||L7|L7|L-----JF7F-JLJILJILJLJFJ|||LJLJF7L-7||LJ|L7F-JI||LJF-JF7LJLJ|L7F7L-7L-7L-JL7L-JF-JL-7FJLJ|L---7FJFJOLJ||OOOO
OOOOOOOOOOOOOF-J|L----7||L-JL-JL--7F7F-J|L------7F7F7IL-J|L-7F-JL--JLJF7|FJL-7FJL7IL--JL---7L7LJL-7|F7L--7FJF--JOF-7LJF-7L----JL7L-7OOLJOOOO
OOOOOOOOOOOOOL-7|F7F--JLJF7F7F-7F7LJLJF7L----7F7||LJ|F7F-JF-JL------7FJLJL-7FJL7FJF7F7F7IF7L7L7F--JLJL---JL-JF7F7L7L7O|FJOF7F---JF7L7OOOOOOO
OOOOOOOOOOOOOF-JLJLJF7F-7|||LJFJ||F7F7||F7F7I||LJ|F-J||L-7L-7F--7IF7|L7F7F7||II||I|LJLJ|FJ|I|FJL----------7F7|LJL-JFJFJL7FJ||F-7FJL-JOOOOOOO
OOOOOOOOOOOOOL------J||FJ|LJOFJFJLJLJLJLJLJL7LJF7||IFJ|F7|F-J|F7L-J|L7LJ||||L7FJL7|F7F-JL7|FJ|IF7F--------J|||F----JOL7FJL7||L7|L--7OOOOOOOO
OOOOOOOOOOOOF7F7F7OF7LJL-JF-7L-JF----------7L7FJLJL7L7||LJL7ILJL--7L7|F-J|||FJ|F-JLJ|L-7I||L7|FJ|L--------7|LJL---7F7FJL7O|||FJL---JOOOOOOOO
OOOOOOOOOF--JLJLJL-JL-----JFJF7OL---------7|FJL---7L-J|L--7L-7IF7IL7||L-7|||L7||F7F-JF-JFJL-J||FJIF7F7F---J|F-----J|LJF-JFJLJL-7F7OF7F7OOOOO
OOOOOOOOOL--7F-7F---7F7F--7L-JL7F----7F7F-J||F-7F7|F-7L-7IL-7|FJL--J||F-J||L7LJLJ|L-7L-7L7F-7|||F7||||L7F7O|L-7F7F7|F-JF-JF-7F7LJ|FJ|||OOOOO
OOOOOOOOOOOFJL7||F--J|LJF7L---7|L---7||LJF7LJL7LJ||L7L--JF7FJ|L-7F-7LJL--JL7L-7F-JF7L7FJILJFJ||LJ||||L7LJL-JF7LJLJLJL--JF-JO||L--J|FJ||OOOOO
OOOOOOOOOOOL7FJLJL---JF-JL----J|F7F-JLJF7|L--7|F-J|FJF7F7||L7|F7LJIL--7F-7FJF-J|F-J|FJ|IIF7L7LJF-J||L7L---7FJ|F--7F--7F7L--7LJF---JL-JL7OOOO
OOOOOOOOOOF7LJF7F----7L--7F7F-7|||L7F-7|||F--J|L7FJL7|||LJ|I||||F7F--7|L7LJIL-7LJF-JL7L7FJL7L7FJIFJ|FJF--7LJFJL-7|L-7||L7F7L--JF7F7F7F-JOOOO
OOOOOOOOOFJL7O||L---7L7F7||LJFJLJL-J|FJ||||F-7L7|L7FJ||L7FJFJLJLJ|L7FJ|FJIF---JF-JF7IL7|L-7|I||F7|FJL-JF7L-7L---JL--JLJFJ|L7F--JLJ||LJOOOOOO
OOOOOOOOOL-7L-JL---7L7LJ|LJF7L7F----JL-JLJ||FJFJL7|L7||FJ|IL-7F--JFJL-J|F7L---7|F7||F7||F-JL7|||||L-7F7|L--JF--7F7F7F7OL7L7|L----7||OOOOOOOO
OOOOOOOOOOOL7F--7F7L-JF7L7FJL-J|OF7F7F7F--J|L7L-7LJFJ|||FJF7FJL7F7L-7F-J|L7F7I||||||||||L7F7||LJ||F-J||L--7O|F-J|||||L-7|FJL-----JLJOOOOOOOO
OOOOOOOOOOOO||F-J|L-7FJL7LJF7F7|FJLJLJLJIF7|FJF-JF-JI||||I||L7FJ||F7|L-7|FJ||FJLJLJ|||||ILJ||L7FJ|L7FJL---JFJL--JLJLJF7||L-7F7OOF7OOOOOOOOOO
OOOOOOOOOOOOLJ|F-JF-J|F7L-7|LJ||L-----7F7|||L7L-7|F-7||||FJL-J|FJ||LJF-J|L7||L-7F--J|||L7F7|L7|L-JFJL------JF7F------JLJ|F7LJL--JL7OOOOOOOOO
OOOOOOOOOOOOF-J|F-JF7||L--J|F-J|F-----J||||L7|F7||L7||||||F---JL7|L7FJF7L7|||F7||F7I|||FJ||L7|L7F-JF7F7F7F-7||L--------7LJL7F----7L7OOOOOOOO
OOOOOOOOOOOOL-7|L--JLJL7F-7|L--JL--7F7FJLJ|FJ||||L7|||||||L--7F7|L7||FJL7|||||||LJ|FJ|||FJL7||FJ|F7|LJ|||L7LJ|F--7F7F7FJOF7|L---7|FJOOOOOOOO
OOOOOOOOOOOOOO||F--7F--J|FJ|F-7F---J||L--7LJOLJLJFJ||||||L7F-J|LJO||LJF-J|||LJ|L-7|L7||||F-J|||FJ|||F-J|L7L7O|L7OLJLJLJF-J|L--7FJ|L7OOOOOOOO
OOOOOOOOOOOOOOLJL-7|L---JL7|L7|L7F-7||F-7L7F-----JFJLJLJ|FJL-7L--7|L-7|F7|||F-JF7||FJ||LJL7FJ|||FJ||L-7L7L7L7L7L---7OF-JF-JF-7|L7L-JOOOOOOOO
OOOOOOOOOOOOOOOOF-JL7OF7F7||FJL7LJFJ|||OL-JL---7F7L-7F--J|OF7|F--JL7FJ||||||L7FJ||||FJL-7O||FJ||L7|L7FJFJO|FJOL-7F7L7L7FJF-JFJ|FJOOOOOOOOOOO
OOOOOOOOOOOOOOOOL--7L7||||LJ|F7|OFJFJ|L----7OF-J|L7FJ|F-7|FJLJL--7FJL7||||||FJL7||LJ|F7FJFJ||FJL-J|FJL-JF-JL---7||L7L-JL-JF7|OLJOF7F7OOOOOOO
OOOOOOOOOOOOOOOOOOOL7LJLJ|F7LJ||FJFJFJF7F-7L7L7FJFJ|OLJFJ||F7F7F-J|F-J||LJ||L7FJ|L7FJ||L7|FJ|L7F7O|L7F--JF-7F7FJ|L7|F-7F--JLJF-7FJLJL-7OOOOO
OOOOOOOOOOOOOOF--7F7L---7LJL7FJ|L7|OL7|LJOL7|FJ|FJFJF7FJFJ|||||L-7|L7OLJOFJ|FJ|FJFJL7||FJLJOL7||L-JFJ|F-7|O|||L7L7|LJFJL---7OL7||F--7FJOOOOO
OOOOOOOOOOOOOOL-7LJL----JF-7|L7L-JL-7|L-7F7LJL7|L7|FJLJFJFJ||||F7|L7L---7|FJL7|L7|F7|||L---7O||L7F7|OLJFJL7LJ|FJO|L-7L7F7F7L--JLJ|OOLJOOOOOO
OOOOOOOOOOOOOOOOL-------7|FJL-JF7F7FJL-7LJL--7|L7LJL--7|FJFJ|||||L7|F---J||OFJL7|||LJ||F--7|FJ|FJ|||F--JF7L7FJL-7|F7|FJ||||F-7F-7L-7OOOOOOOO
OOOOOOOOOOOOOOOOF------7LJL7F7FJ|||L--7|F-7F-JL-JF----J|L7|FJ|LJ|FJ|L--7FJL7|F7|||L7FJ|L-7||L7|L7|LJL7F7|L-JL7F-JLJLJL7||||L7||FJF-JOOOOOOOO
OOOOOOOOOOOOOOOOL-----7L---J|||FJ||F7FJ|L7|L--7OFJF-7F7L7||L7L7O|L7|F7FJL7FJ||LJLJFJL7|F7||L7||FJ|F--J||L---7|L---7F7FJ||||FJ||L7L7OOOOOOOOO
OOOOOOOOOOOOOOF7F7F7F7L----7||||FJ||LJOL7||F--JFJFJFJ|L7|||FJFJFJFJ|||L7FJ|O||OF--JF-J||LJ|FJ||L7|L-7FJ|F---JL7F-7LJ||FJ||LJOLJO|FJOOOOOOOOO
OOOOOOOOOOOOOO|LJLJLJL-----J|||||O|L--7FJ||L--7L-JO|FJFJ|||L-JFJFJFJ|L-JL7L7||FJF7FJF7||F-J|FJL7LJF-J|FJL7F--7|L7|F-JLJO||OOOOF-JL7OOOOOOOOO
OOOOOOOOOOOOOOL-7F---------7||||L7|F--JL-JL-7FJF---JL7L-J||F--JFJOL7L---7L7|||L7|||O|LJ|L-7||F7|F7|F7|L-7LJF7||FJ|L----7|L7OOFJF--JOOOOOOOOO
OOOOOOOOOOOOOOOOLJOF7F-----J|||L-J|L------7FJ|FJF7F--JF--J||F7FJF7O|F7F7L7||LJO||LJFJF7L7O||LJ|||LJ|||F-JF-JLJ|L7|F7F7FJ|FJOOL-JOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOF-JLJF7F--7||L-7FJF7F7F--JL-JL7||L-7OL-7FJ|||L-J|FJ|||L7|LJF--JL7FJFJ|FJFJ|F7LJL-7|LJL-7L-7F-JFJLJLJ|L7||OOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOFJF7F7|||F-J||F7||FJ|||L---7OF7LJ|F7L-7FJL7|||F--JL7|||FJ|F7|F-7FJL7L7||FJFJ||F7F-JL-7OFJF-JL-7L--7OFJFJ||OOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOL7|LJ|||||F7||||||L7|||F-7FJFJL--J||F-JL7FJLJ||F--7LJ||L7LJ|||FJ|F-JFJ||L7|FJLJLJF-7FJFJFJOF-7L7F7L7|FJOLJOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOLJF-J|||||||LJ|||FJ||||O|L7|F-7F7|||OF-JL-7FJLJF-JF7LJOL-7|LJ|FJ|F-JO||FJ|L7F7F-JFJ|OL7|F7|FJO|||FJ||OOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOL-7||||||L7OLJ||FJ||L7L-J||FJ|||||FJF--7||F-7L--JL-7F--JL-7|L7||OOO|||FJOLJ||F-JFJF-JLJLJL-7||LJO|L-7OOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOO||LJ||L7L7F7LJL7|L7L-7FJ|L-J|||||FJF-J|||FJF-7F7FJL--7F7|L-JLJOOOLJ||F---J||F7L7L7F7F7F7FJ|L7OOL7FJOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOLJOFJL7L7LJL--7LJOL7FJ|FJF--J||||L7L-7|||L7|O||LJOOOOLJLJOOOOOOOOOOLJL---7||||FJO||||||LJO|FJOOOLJOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOL7FJFJF-7F7|OOOO||O|L7L---JLJL7L-7||||FJL7|L--7OOOOOOOOOOOOOOOOOOOOF--J||||L-7LJ|||L7OO||OOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOF-JL7L7L7LJ|L-7OO||FJFJF-------JF-J|||||F7||F-7L7OOOOOOOOOOOOOOOOOOOL-7FJ|||F-JOO|||FJOOLJOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOL7F7|OL7L7OL--JOO|||FJFJF7F7F7F7L-7|||||||||L7L-JOOOOOOOOOOOOOOOOOOOOO||FJ||L-7OOLJ||OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOO||LJOOL7L-7OOOOOLJLJFJFJ|||||||F7|LJLJLJLJL-JOOOOOOOOOOOOOOOOOOOOOOOFJ||FJ|F7|OOOOLJOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOLJOOOOFJF-JOOOOOOOOOL-JO||||||||||OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO|FJ||OLJLJOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOL7L7OOOOOOOOOOOOOOLJ||||||||OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOF--JL7LJOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOL7L7OOOOOOOOOOOOOOOLJLJ||LJOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOL----JOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOL-JOOOOOOOOOOOOOOOOOOO||OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOLJOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
```

Good day, this Write-up is 1035 lines long! 😻
