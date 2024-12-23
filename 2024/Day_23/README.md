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

## The end part  

Maybe I’ll code the algorithm myself. Maybe.  

I’m happy to have learned a new concept, though—I didn’t know what a clique was before!  
