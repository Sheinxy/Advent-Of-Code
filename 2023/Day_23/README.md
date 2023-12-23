## Day 23

This year's [longest path problem](https://en.wikipedia.org/wiki/Longest_path_problem) is finally here!

## The input:

In order to represent the input, I have a data structure with three fields:
 - The starting point (the only '.' on the first row)
 - The end point (the only '.' on the last row)
 - The grid (which I represent as a [Data.Matrix](https://hackage.haskell.org/package/matrix-0.3.6.3/docs/Data-Matrix.html) as always)

```hs
data Input  = Input { start :: (Int, Int), end :: (Int, Int), grid :: Matrix Char } deriving (Show)

parseInput :: String -> Input
parseInput input = Input { start=start, end=end, grid=grid }
    where grid  = (fromLists . lines) input
          start = head [(1         , c) | c <- [1 .. ncols grid], grid ! (1         , c) == '.']
          end   = head [(nrows grid, c) | c <- [1 .. ncols grid], grid ! (nrows grid, c) == '.']
```

## Getting to know your neighbours:

By now, finding neighbours functions are pretty common, so I won't spend much time describing it! The basic idea is:
 - I have a boolean which enable slippery tiles (if True then find neighbours the part 1 way, otherwise find them the part 2 way)
 - Get the neighbours for the current position (accounting for slipperiness)
 - Keep the ones that are in the grid and that aren't walls

```hs
getNeighbours :: Bool -> (Int, Int) -> Matrix Char -> [(Int, Int)]
getNeighbours isSlippy pos@(r, c) grid = filter isNotWall . filter isInGrid $ neighbours
    where char             = grid ! pos
          isInGrid  (r, c) = 0 < r && r <= nrows grid && 0 < c && c <= ncols grid
          isNotWall (r, c) = grid ! (r, c) /= '#'
          neighbours | isSlippy && char == '>' = [(r, c + 1)]
                     | isSlippy && char == '<' = [(r, c - 1)]
                     | isSlippy && char == 'v' = [(r + 1, c)]
                     | isSlippy && char == '^' = [(r - 1, c)]
                     | otherwise               = [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]
```

## Solving a NP-HARD problem for dummies:

The longest path problem is [NP-HARD](https://en.wikipedia.org/wiki/NP-hard). If you don't know what that means (don't worry, I don't exactly know either), basically remember that there is **HARD** in the name :).

So, the best approach that we can have here is to brute-force our way through:
 - Try every possible paths.

This works quite well on the example inputs, and it runs decently fast on the actual input for part 1.

However, if you try to naively bruteforce your way through part 2, you're going to have to wait at least an hour!

And that's only if you have a good computer! (Which is not my case!)

So, we need to find some optimisation here. My first thought was to try caching some results. However, caching with the visited tiles in my key proved too slow, and caching with the path length in my key didn't give correct results.

After some time, I decided to analyse the input, and I noticed something quite interesting:
 - The input is mostly composed of straight lines, with a few junctions (or intersections if you prefer) here and there!

So, silly as I was, I thought about caching using the visited junctions. This was too slow, so I went to have breakfast.

After my breakfast, I realised something: I can simplify my grid in a weighted graph of connected junctions!

Basically:
```
#.#########
#.........#
#.#######.#
#.######..#
#........##
########.##
```

Here, there are four junctions:
 - The starting and ending tiles (S, E)
 - The tile at the second row, second column (A)
 - The tile at the first before last row, second before last column (B)

So I can represent my grid with a weight graph:
 - S -> A with weight 1
 - A -> B with weight 12
 - B -> E with weight 1

This greatly simplifies my problem, as I now only have to find the longest path in a graph with 4 nodes!

On my actual input, this changes my number of nodes from about 9500 to just 36!

This also automatically prunes dead ends! For example:
```
#.#############
#......########
#.####..#######
#.#####.#######
#.####..#######
#.####.########
#.####........#
#.#############
```

Here, it is quite obvious that following the right (as in going Eastbound first, not as in "the correct") path will lead to a dead end!
However, when creating our junction graph, we get the following junctions:

```
#S#############
#A.....########
#.####..#######
#.#####.#######
#.####..#######
#.####.########
#.####........#
#E#############
```

In graph form:
 - S -> A
 - A -> E

This means that, instead of following two paths, we only have one path to follow! (Which is quite fast, as the longest path is now the only one)

---

Now, my initial way of doing creating this graph was to simply launch a [bfs](https://en.wikipedia.org/wiki/Breadth-first_search) from the starting junction.
The encountered junctions would not propagate the bfs further, and I keep the first found distance as the weight between two junctions.

```hs
type WGraph = M.Map (Int, Int) [(Int, (Int, Int))] -- Weighted directed graph. Edges are represented by a list of (cost, successor)

makeJunctionGraph :: Bool -> Input -> WGraph
makeJunctionGraph isSlippy (Input start end grid) = treatJunctions (S.singleton start) M.empty [start]
    where isJunction pos | pos == start || pos == end = True
                         | otherwise                  = length (getNeighbours isSlippy pos grid) > 2
          treatJunctions _ graph [] = graph
          treatJunctions seen graph (x:queue) = treatJunctions seen' graph' queue'
            where nextJunctions = findNextJunctions (S.singleton x) [(0, x)]
                  graph'    = M.insert x nextJunctions graph
                  junctions = filter (`S.notMember` seen) $ map snd nextJunctions
                  seen'     = foldr S.insert seen junctions
                  queue'    = queue ++ junctions
          findNextJunctions _ [] = []
          findNextJunctions seen ((d, x):queue) | d > 0 && isJunction x = (d, x) : findNextJunctions seen queue
                                                | otherwise             =          findNextJunctions seen' queue'
            where neighbours = filter (`S.notMember` seen) $ getNeighbours isSlippy x grid
                  seen'      = foldr S.insert seen neighbours
                  queue'     = queue ++ zip (repeat (d + 1)) neighbours
```

My makeJunctionGraph function here creates the graph of junctions from an input. It works with a queue of junctions to treat:
 - While the queue is not empty:
   - Pop the first junction from the queue
   - Launch a bfs starting from that junction to find the next junctions and their distance
   - Add these junctions to the queue and to the graph

What the bfs does is:
 - While the queue is not empty:
   - Pop a tile from the queue
   - If it is a junction (and not the starting one), add it to the result
   - Otherwise, continue the bfs on the neighbouring tiles

This works on my input (and probably most inputs) because it has one interesting property:
 - There is only one way (or at least on distance) to go from one junction to another!

This, however, is not the case for the first example showed above! It would tell me that the distance between A and B is 11, not 12! (it would take the lower path, at it is the shortest path)

I fixed this as soon as I noticed it:
```hs
type WGraph = M.Map (Int, Int) [((Int, Int), Int)] -- Weighted directed graph. Edges are represented by a list of (successor, cost)

makeJunctionGraph :: Bool -> Input -> WGraph
makeJunctionGraph isSlippy (Input start end grid) = treatJunctions (S.singleton start) M.empty [start]
    where isJunction pos | pos == start || pos == end = True
                         | otherwise                  = length (getNeighbours isSlippy pos grid) > 2
          treatJunctions _ graph [] = graph
          treatJunctions seen graph (x:queue) = treatJunctions seen' graph' queue'
            where nextJunctions = M.assocs $ findNextJunctions (S.singleton x) [(x, 0)] M.empty
                  graph'    = M.insert x nextJunctions graph
                  junctions = filter (`S.notMember` seen) $ map fst nextJunctions
                  seen'     = foldr S.insert seen junctions
                  queue'    = queue ++ junctions
          findNextJunctions _ [] found = found
          findNextJunctions seen ((x, d):queue) found | d > 0 && isJunction x = findNextJunctions seen queue found'
                                                      | otherwise             = findNextJunctions seen' queue' found
            where neighbours = filter (`S.notMember` seen) $ getNeighbours isSlippy x grid
                  seen'      = foldr S.insert seen $ filter (not . isJunction) neighbours
                  queue'     = queue ++ zip neighbours (repeat (d + 1)) 
                  found'     | x `M.member` found = M.adjust (max d) x found -- There is another way to get to this junction: keep the longest
                             | otherwise          = M.insert x d found
```

The code is almost the same, to one small difference:
 - I allow junctions to be visited multiple times during my bfs.
 - When I encounter a junction for a second time, I set my result as the longest distance (which should always be the new one when encountering it again)

My code for that could be cleaner, but I quickly hacked that fix in because I didn't have much time :C (at least it works and doesn't deter performances!)

Now that I have a way to create my junction graph, all I need to do is to brute-force every possible path and to keep the best result:

```hs
findLongestPath :: Bool -> Input -> Output
findLongestPath isSlippy input = go S.empty 0 (start input)
    where graph = makeJunctionGraph isSlippy input
          go seen pathLen cur | cur == end input = pathLen
                              | otherwise        = best
                              where seen'        = S.insert cur seen
                                    neighbours   = [(pos, dist) | (pos, dist) <- graph M.! cur, pos `S.notMember` seen]
                                    best         = maximum (0 : parMap rseq (\(p, d) -> go seen' (pathLen + d) p) neighbours)
```

I do this using a [dfs](https://en.wikipedia.org/wiki/Depth-first_search): 
 - I start at the starting junction.
 - When I'm visiting a junction:
   - If it is the ending junction, I simply return the length of my path
   - otherwise:
     - I mark it as visited for the subsequent recursions
     - I get the neighbours for that junction (as well as the longest distance to get there)
     - I recurse (visit) each of the neighbours, and I return the best result

Note that I do my recursion in parallel to go a little bit faster!

And now, solving part one and part two is simply calling the findLongestPath function, with and without slipperiness! 

```hs
partOne :: Input -> Output
partOne = findLongestPath True

partTwo :: Input -> Output
partTwo = findLongestPath False
```

Done!
