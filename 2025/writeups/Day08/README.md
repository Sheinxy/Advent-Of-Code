## Day 08

I might have gone a bit overboard with this one lol.

## The Input

The input is just a list of three-tuples. Haskell already knows
how to parse that, as long as there are parenthesis around them :)

```hs
type Position = (Int, Int, Int)
type Input = [Position]
type Output = Int

parseInput :: String -> Input
parseInput = map (\x -> read $ "(" ++ x ++ ")") . lines
```

## Part One

### The Problem

We want to connect the 1000 closest pairs of numbers together and multiply
the sizes of the three biggest components that this creates.

### The Solution

We have two subproblems here:
1. How do we get the closest pairs in the right order?
2. How do we efficiently represent the graph as we make it?

Let's start with subproblem 1.

This is pretty simple to do in Haskell, here is a way of doing it (albeit
unefficient).

```hs
distance :: Position -> Position -> Int
distance (x1, y1, z1) (x2, y2, z2) = x * x + y * y + z * z
    where x = x1 - x2
          y = y1 - y2
          z = z1 - z2

closestPairs :: Input -> [(Position, Position)]
closestPairs boxes = sortBy (comparing $ uncurry distance)
                     [(b1, b2) | b1 <- boxes, b2 <- boxes, b1 < b2]
```

distance simply computes the distance (squared) between two positions.

closestPairs goes through every ordered pair and sorts them by their distance.

Now, subproblem number two is pretty fun: the real challenge isn't in the algorithm, but in the data representation.

The algorithm itself is pretty simple:
- Get the closest pair that hasn't been treated yet
- Insert the edge in the graph
Repeated 1000 times

The real problem comes to the way we represent the graph:
- Do we represent the full graph?
- Do we only keep track of its components?

Here, I chose the latter. I made a small data structure that I called component map:
```hs
-- A data structure representing the connected components of an
-- undirected graph, without storing any of the graph’s edges.
data ComponentMap k = ComponentMap
  { idMap   :: M.Map k Int
  , sizeMap :: M.Map Int Int
  } deriving (Show, Eq)
```

Technically speaking, sizeMap can be derived from idMap, but I think having a
fast O(log n) access to the sizes is nice.

In a component map, I can merge two components. That is:
- Every node belonging to component 2 now belogs to component id1
- The size of component id1 increases by the size of component id2
- Component id2 gets emptied.

```hs
mergeComponents :: Ord k => ComponentMap k -> Int -> Int -> ComponentMap k
mergeComponents (ComponentMap mIds mSize) id1 id2 = ComponentMap mIds' mSize'
    where (idMerge, idVoid) =  (min id1 id2, max id1 id2)
          mIds'  = M.map (\x -> if x == idVoid then idMerge else x) mIds
          mSize' = M.insert idVoid 0
                 . M.adjust (\x -> x + (mSize ! idVoid)) idMerge
                 $ mSize
```

(I could and should actually delete component idVoid here, but it makes it simple
to simply "empty" it as it means that I can just use Size mSize for new IDs
when inserting edges)

Now, in order to insert a new edges there are four possible cases:
- Both vertices of the edge are new -> Create a new component and add them inside
- One vertex exists but not the other -> Add the other inside the first one's component
- Both vertices exist and are in the same component -> Do nothing, this edge doesn't change the components
- Both vetices exist but are in two different components -> Merge the components

```hs
insertEdge :: Ord k => ComponentMap k -> (k, k) -> ComponentMap k
insertEdge cMap@(ComponentMap mIds mSize) (a, b) =
    case (mIds !? a, mIds !? b) of
        (Nothing, Nothing) ->
            let newID  = M.size mSize
                mIds' = foldl (\m p -> M.insert p newID m) mIds [a, b]
                mSize' = M.insert newID 2 mSize
            in ComponentMap mIds' mSize'
        (Just idVal, Nothing) ->
            ComponentMap
                (M.insert b idVal mIds)
                (M.adjust (+ 1) idVal mSize)
        (Nothing, Just idVal) ->
            ComponentMap
                (M.insert a idVal mIds)
                (M.adjust (+ 1) idVal mSize)
        (Just id1, Just id2)
            | id1 == id2 -> cMap
            | otherwise -> mergeComponents cMap id1 id2
```

With that, part one is just a question of inserting the first 1000 pairs,
then taking the 3 biggest components and multiplying their sizes:
```hs
partOne :: Input -> Output
partOne = product . take 3 . sortBy (comparing Down) . M.elems . sizeMap
        . foldl insertEdge (ComponentMap M.empty M.empty)
        . take 1000 . closestPairs
```

## Part Two

### The Problem

We want to insert edges until we just have one single component containing all
the nodes.

### The Solution

One nice thing I did in my mergeComponents function is that I'm always using
the smallest ID as the merging component.

This means that we can just insertEdges until the size of component 0 is
equal to the number of positions.

When we have that, we know exactly which edge caused this:

```hs
partTwo :: Input -> Output
partTwo boxes = x1 * x2
    where numBoxes = length boxes
          ((x1, _, _), (x2, _, _)) = closestPairs boxes !! (n - 1)
          n = length
            . takeWhile (\m -> sizeMap m !? 0 /= Just numBoxes)
            . scanl insertEdge (ComponentMap M.empty M.empty)
            $ closestPairs boxes
```

## Conclusion

This was technically overkilled. Lists would have done the trick.

But maybe I'll reuse this data structure some day, who knows?
