## Day 23  

Part 2 be like:  
```hs
import Thing.That.Solves.The.Problem (solveTheProblem)

partTwo :: Input -> String
partTwo = solveTheProblem
```

---

## The input  

The input is a list of edges in the form "a-b".  

I represent this graph as a [Data.Map](https://hackage.haskell.org/package/containers-0.7/docs/Data-Map-Strict.html).  
The keys of my Map are the nodes, and the values are the lists of adjacent nodes.  

To parse the input, I start by retrieving each edge by splitting the input into lines,  
and then splitting each line by "-".  

Next, I create a Map, starting from an empty one. For each edge "a-b", I add  
b to the adjacent nodes of a, and a to the adjacent nodes of b.  

```hs
parseInput :: String -> Input
parseInput input = graph
    where alterVertex a Nothing   = Just [a]
          alterVertex a (Just xs) = Just (a : xs)
          addMap [a, b] m = Map.alter (alterVertex a) b $ Map.alter (alterVertex b) a m
          edges = map (splitOn "-") . lines $ input
          graph = foldr addMap Map.empty edges
```

---

## Part 1  

### The problem  

We need to find all the loops of three nodes where at least one of the nodes starts with a t.  

---

### The solution  

#### Solving a subproblem first  

Let’s start with this subproblem:  
Given a node starting with t, find all loops of three nodes containing it.  

To do this, I iterate through each (ordered) pair (a, b) of  
adjacent vertices to my t-node:  
- t - a - b is a loop if the edges t-a, t-b, and a-b all exist.  
- Since a and b are adjacent to t by definition, we only need to check if  
  the edge a-b exists.  
- Because the graph is undirected, this simply means checking if b is in the adjacency list of a.  

```hs
findConnected :: Input -> String -> [[String]]
findConnected graph t = [sort [t, a, b] | a <- neighbours,
                                          b <- neighbours,
                                          a < b,
                                          b `elem` (graph Map.! a)]
    where neighbours = graph Map.! t
```

---

Note: I sort the loop to get a "canonical" form.  

---

#### Getting all the loops  

Now that we can find all the loops for a given t-node,  
we can find all the loops for all t-nodes.  

```hs
partOne :: Input -> Output
partOne input = length . nub $ concatMap (findConnected input) ts
    where ts = filter (('t' ==) . head) $ Map.keys input
```

---

## Part 2  

### The problem  

We need to find the largest subset of nodes such that all nodes in that subset  
are adjacent to each other.  

---

### The solution  

A subset of nodes in a graph where all nodes are adjacent to each other is called a  
[clique](https://en.wikipedia.org/wiki/Clique_(graph_theory)).  

What we need to find is the largest maximal clique, which is a clique  
that is not a subset of any other clique.  

Finding maximal cliques can be done using the  
[Bron-Kerbosch algorithm](https://en.wikipedia.org/wiki/Bron–Kerbosch_algorithm).  

This isn’t too hard to implement in Haskell, but why reinvent the wheel  
when the [Data.Algorithms.MaximalCliques](https://hackage.haskell.org/package/maximal-cliques-0.1.1/docs/Data-Algorithm-MaximalCliques.html)  
library exists?  

All we need to do to use that library is provide two things:  
1. A function that checks whether a node is adjacent to another:  
```hs
          isAdjacent a = (`elem` (input Map.! a))
```
2. A list of nodes:  
```hs
    where nodes = Map.keys input
```

---

```hs
partTwo :: Input -> String
partTwo input = intercalate "," . sort $ maxClique
    where nodes = Map.keys input
          isAdjacent a = (`elem` (input Map.! a))
          maxClique = maximumBy (compare `on` length) $ getMaximalCliques isAdjacent nodes
```

---

## Bonus

Let's code the most basic implementation of the algorithm!

```hs
bronKerbosch :: Input -> [[String]]
bronKerbosch graph = bronKerbosch' Set.empty nodes Set.empty
    where nodes = Set.fromList $ Map.keys graph
          bronKerbosch' :: Set String -> Set String -> Set String -> [[String]]
          bronKerbosch' r p x | Set.null p && Set.null x = [Set.toList r]
                              | otherwise = res
            where (_, _, res) = Set.foldr go (x, p, []) p
                  go v (x, p, res) = (Set.insert v x, Set.delete v p, res ++ bronKerbosch' r' p' x')
                    where r' = Set.insert v r
                          p' = Set.intersection p $ Set.fromList (graph Map.! v)
                          x' = Set.intersection x $ Set.fromList (graph Map.! v)

partBonus :: Input -> String
partBonus = intercalate "," . sort .
            maximumBy (compare `on` length) .
            bronKerbosch
```

The runtime is a bit slower (which is expected, notably because this implementation
doesn't use any optimisation).

```
➜  Day_23 git:(main) ✗ stime ./Day_23 bonus input
bs,cf,cn,gb,gk,jf,mp,qk,qo,st,ti,uc,xw
        0.56 real         0.53 user         0.01 sys
➜  Day_23 git:(main) ✗ stime ./Day_23 two input
bs,cf,cn,gb,gk,jf,mp,qk,qo,st,ti,uc,xw
        0.08 real         0.06 user         0.01 sys
```

## The end part  

Maybe I’ll code the algorithm myself. Maybe.  
**Update: I did.**

I’m happy to have learned a new concept, though—I didn’t know what a clique was before!  
