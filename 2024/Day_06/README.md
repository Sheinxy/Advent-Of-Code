## Day 06

2D grids manipulation is back on the menu!

![Scared cat](https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Ffarm5.staticflickr.com%2F4058%2F4553277701_7d74c5ae92_o_d.jpg&f=1&nofb=1&ipt=9c2413e3be4a44d42e9125750abbaa4a3ed29769ef0e2cd62c17e7e1c792209b&ipo=images)

Also, bruteforce goes brrrrrrrrrrrrr

## The input

The input is pretty simple: we have a 2D grid where '#' are obstacles,
and a guard is represented with '^<>v' (depending on what direction it's facing.

What we really need to keep track of in order to solve this problem is:
- The position of each obstacle
- The position of the guard
- The direction the guard is facing
- The dimensions of the grid

I packed all this information inside a single datatype:

```hs
data Heading = North | South | West | East deriving (Show, Eq, Ord)
data World = World { obstacles :: Set (Int, Int),
                     position :: (Int, Int), 
                     heading :: Heading,
                     height :: Int, width :: Int } deriving Show
```

Getting the grid is a matter of splitting by lines, which is easy.
Once I have the grid, I get its length in order to get the height, and the length
of the first line for the width.

```hs
where grid   = lines input
      height = length grid
      width  = length . head $ grid
```

Next, I index each element of the grid in order to easily get the position of
interesting elements. I do this by abusing zips:
```hs
  indexedLines = concat $ zipWith
                 (\i s -> ([(i, j, c) | (j, c) <- zip [0 .. ] s])) [0 .. ]
                 grid 
```

Basically, if my input is
```
ab
de
```
indexedLines will be
```
[(0, 0, 'a'), (0, 1, 'b'), (1, 0, 'd'), (1, 1, 'e')]
```

Now I simply filter the indexed grid in order to retrieve the positions of all obstacles and of the guard:
```hs
  (start, c)   = head [((i, j), c) | (i, j, c) <- indexedLines, c `notElem` ".#"]
  obstacles    = Set.fromList [(i, j) | (i, j, c) <- indexedLines, c == '#']
  heading | c == '^'  = North
          | c == 'v'  = South
          | c == '>'  = East
          | otherwise = West
```

Note that I am keeping the obstacles as a Set in order to have a faster lookup.

Putting all of this together:
```hs
parseInput :: String -> Input
parseInput input = World obstacles start heading height width
    where grid   = lines input
          height = length grid
          width  = length . head $ grid
          indexedLines = concat $ zipWith
                         (\i s -> ([(i, j, c) | (j, c) <- zip [0 .. ] s])) [0 .. ]
                         grid 
          (start, c)   = head [((i, j), c) | (i, j, c) <- indexedLines, c `notElem` ".#"]
          obstacles    = Set.fromList [(i, j) | (i, j, c) <- indexedLines, c == '#']
          heading | c == '^'  = North
                  | c == 'v'  = South
                  | c == '>'  = East
                  | otherwise = West
```

## Part 1:

### The problem

I want to trace the path that the guard takes.

### The solution

Let's start by focusing on a smaller problem:
I want to trace the guard walking in one direction.

In order to do that, I simply get all the tiles going from the guard's position
to the edge of the grid, and I take them while they're not part of the obstacle set.

```hs
getGuardWalkLine :: World -> [(Int, Int)]
getGuardWalkLine w = takeWhile (`Set.notMember` obs) steps
    where obs     = obstacles w
          (i, j)  = position w
          steps | heading w == North = zip (reverse [0 .. i]) (repeat j)
                | heading w == South = zip [i .. height w - 1] (repeat j)
                | heading w == East  = zip (repeat i) [j .. width w - 1]
                | otherwise          = zip (repeat i) (reverse [0 .. j])
```

Now that I'm able to trace the line made from the guard walking in a single direction,
I want to trace the whole path.

All I have to do is trace the guard walk line until its position is that of an edge, while
updating its position, heading, and keeping track of which tiles have been visited:

```hs
rotate :: Heading -> Heading
rotate North = East
rotate East  = South
rotate South = West
rotate West  = North

isOnEdge :: World -> (Int, Int) -> Bool
isOnEdge w (i, j) = i == 0 || i == height w - 1 || j == 0 || j == width w - 1

getGuardPath :: World -> Set (Int, Int)
getGuardPath world = snd $
                     until isDone go
                     (world, Set.empty)
    where isDone (w, _) = isOnEdge w (position w)
          go (w, v) = (w { position = position', heading = heading'}, v')
            where visited = getGuardWalkLine w
                  position' = last visited
                  heading'  = rotate $ heading w
                  v'        = foldr Set.insert v visited
```

`until isDone go` will iterate using the go function until isDone returns True.

The go function calls the getGuardWalkLine on the current state of the world,
retrieving the path walked by the guard by going towards its current facing direction.

It then computes the new position (which is the last tile the guard visited), as
well as it's new facing direction (by rotation its previous one).

Finally, it records all the visited tiles.

With the set of all visited tiles in our hand, we can simply retrieve its size
to get our answer:
```hs
partOne :: Input -> Output
partOne = Set.size . getGuardPath
```

## Part 2:

### The problem

Now that we know the path visited by the guard, we need to find the number of ways
we can make this path into a loop by adding a single obstacle.

### The solution

Bruteforce.

The only places where it makes sense to add an obstacle are the ones on the visited path,
so let's start by getting that:

```hs
    where guardPath   = Set.toList $ Set.delete (position input) (getGuardPath input)
```

Note that I remove the starting position, as we can't block that one.

Now, the path has formed a loop if none of them are on the edge of the grid
```hs
          isLoop = not . any (isOnEdge input). Set.toList
```

Now, we can try blocking a position and checking if it forms a loop when getting the path:
          tryBlocking pos = isLoop (getGuardPath w)
            where w = input { obstacles = Set.insert pos (obstacles input) }
```

PROBLEM! We need to update our getGuardPath function to stops when it detected a loop!

In order to do that, we can simply keep track of all the encountered positions of our guard (with its heading),
and we stop when we fall back onto a known position!

```hs
getGuardPath :: World -> Set (Int, Int)
getGuardPath world = snd3 $
                     until isDone go
                     (world, Set.empty, Set.empty)
    where isDone (w, _, p) = isOnEdge w (position w) || (position w, heading w) `Set.member` p
          go (w, v, pos) = (w { position = position', heading = heading'}, v', pos')
            where visited = getGuardWalkLine w
                  position' = last visited
                  heading'  = rotate $ heading w
                  v'        = foldr Set.insert v visited
                  pos'      = Set.insert (position w, heading w) pos
```

Now, we can simply try blocking all blocks, and only keep the ones that form a loop:
```hs
partTwo :: Input -> Output
partTwo input = length $ filter tryBlocking guardPath 
    where guardPath   = Set.toList $ Set.delete (position input) (getGuardPath input)
          isLoop = not . any (isOnEdge input). Set.toList
          tryBlocking pos = isLoop (getGuardPath w)
            where w = input { obstacles = Set.insert pos (obstacles input) }
```

### Bonus

You know, there isn't any reason for us to try blocking each position in sequential order.
The result of one computation won't impact that of the next one.

We can try throwing multithreading into the mix!

```hs
partTwo :: Input -> Output
partTwo input = length . filter id $ parMap rseq tryBlocking guardPath 
    where guardPath   = Set.toList $ Set.delete (position input) (getGuardPath input)
          isLoop = not . any (isOnEdge input). Set.toList
          tryBlocking pos = isLoop (getGuardPath w)
            where w = input { obstacles = Set.insert pos (obstacles input) }
```

## The end part

Bruteforcing goes brrrrrrrrrrrrrrr!

Once again, using a Data.Array or Data.Matrix would've been better in order to keep track of the obstacles,
as they have O(1) index-based access.

But I just didn't feel like it...
