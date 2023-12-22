## Day 22

Today was a fun day! Not too hard, and quite fun!

## Bricks:

In order to represent bricks, I use the following data structure:

```hs
data Brick = Brick { minZ :: Int, maxZ :: Int, cubes :: S.Set (Int, Int, Int), supporting :: S.Set Int} deriving (Eq, Ord)
```

Here is what each field means:
 - minZ is the bottom z coordinate of the brick.
 - maxZ is the top z coordinate of the brick
 - cubes is a set of all cube coordinates forming the brick
 - supporting is the set of all bricks (represented by their index) on which this brick is laying on top of.

minZ and maxZ are my first field in my structure, simply because I want to compare bricks by their z coordinate first and foremost.

This will be very useful later!

## Parsing a brick from a line:

For once, instead of making a function for parsing the input, I simply make Brick an instance of Read (allowing me to simply call the read function on a line of my input file):

```hs
instance Read Brick where
    readsPrec _ s = [(Brick minZ maxZ cubes S.empty, "")]
        where (edge1, '~':edge2)           = break (=='~') . head . words $ s
              ((x1, y1, z1), (x2, y2, z2)) = both (read . ('(' :) . (++ ")")) (edge1, edge2)
              minZ  = min z1 z2
              maxZ  = max z1 z2
              cubes = S.fromList [(x, y, z) | x <- [min x1 x2 .. max x1 x2]
                                            , y <- [min y1 y2 .. max y1 y2]
                                            , z <- [min z1 z2 .. max z1 z2]]
```

The way this works is:
 - I start by taking everything before the first space (you'll see why later)
 - I split that using the '~' sign as a separator. This gives me two strings: "x1,y1,z1" and "z2,y2,z2"
 - I add parenthesis around these strings and parse them as 3-tuples
 - I find my minimum z value and my maximum z value
 - I generate all the cubes forming the brick

To have some fun, I've also made Brick an instance of Show:

```hs
-- This one is just for fun (and easier debugging in case there are like 1000 cubes in a brick)
instance Show Brick where
    show (Brick _ _ c s) = firstCube ++ "~" ++ lastCube ++
                            " | Number of cubes = " ++ show (S.size c) ++
                            " | Supported by = "    ++ show s
        where firstCube = init . tail . show $ S.findMin c
              lastCube  = init . tail . show $ S.findMax c
```

This allows me to pretty print my input in a format that is similar to the original input format.

Interesting to note here is that (read . show) brick == id brick (because I split by spaces first in my readsPrec!) :D

## Making them fall down:

Our list of initial state bricks is actually quite uninteresting for us. What we really want to work with is the list of bricks **AFTER** they've fallen down. This is the meatiest part of my solution, so here we go!

The basic idea is the following:
 - I treat each brick from lowest to highest:
   - I move down one step
   - If I am inside the ground, our inside some bricks (that have already been treated), then move back up one step and add the ground/the touched bricks as the supporting bricks of the current brick (that's a lot of bricks c:)
   - Otherwise, go down one step and repeat

While this idea works, it is not the fastest! One simple speedup that can be done (and that I did) is that:
 - Instead of moving down one step at a time, move down the the next highest z value of the treated bricks (the top of the tower as seen so far). By definition, there will be no supporting bricks above that value, therefore we don't need to check them and we can skip directly to that value!

Put that into code:
```hs
fallDown :: [Brick] -> M.Map Int Brick
fallDown = foldl' go M.empty
    where ground = S.singleton (-1)  -- Singleton for bricks supported only by the ground
          go fall cur | z == 0        = M.insert i (cur { minZ=z+1 , maxZ=mz+1 , cubes=backCubes, supporting=ground }) fall -- Touched the ground
                      | null touching = go fall    (cur { minZ=z+dz, maxZ=mz+dz, cubes=nextCubes }) -- Touched no bricks
                      | otherwise     = M.insert i (cur { minZ=z+1 , maxZ=mz+1 , cubes=backCubes, supporting=touching }) fall
                      where i         = M.size fall -- The index for the current brick
                            z         = minZ cur    -- The bottom z value of the brick
                            mz        = maxZ cur    -- The top    z value of the brick
                            -- Speeding up the process by going more that one step at a time
                            nextZ     = maximum $ 0 : [maxZ c | c <- M.elems fall, maxZ c < z]
                            dz        = nextZ - z
                            -- Find the supporting bricks at that z step
                            touching  = M.keysSet . M.filter (not . S.disjoint (cubes cur) . cubes) $ fall
                            -- Get the cubes for going up or continuing falling done
                            backCubes = S.map (third3 succ ) . cubes $ cur
                            nextCubes = S.map (third3 (+dz)) . cubes $ cur
```

## And now, we put all of that together:

Here is now my parseInput function:
```hs
parseInput :: String -> Input
parseInput = fallDown . sort . map read . lines
```

## What can we remove?:

Part one is about finding which bricks can be safely removed. A brick can be removed if:
 - Removing it won't make another brick fall down.

That is:
 - For all bricks supported by this brick, there are other bricks supporting them.

My claim is that, with the way I represent my data, it is easier to find bricks that are **NOT** safe to remove.

Then to find the ones that are safe to remove, we simply need to take the complement set of our bricks (and because we only care about sizes, we actually need to subtract the number of total bricks by the number of unremovable bricks)

To find unremovable bricks, I simply go through all of my bricks:
 - If this brick has only one other brick supporting it, then I put that supporting brick in my resulting set

This, however, will also put the ground as an unremovable brick (which is, ultimately, not false, however it would make things more annoying to work with), therefore I remove it from my set.

```hs
getUnremovableBricks :: Input -> S.Set Int
getUnremovableBricks = S.filter (-1 /=) . M.foldr go S.empty
   where go cur acc | (S.size . supporting) cur == 1 = acc `S.union` supporting cur
                    | otherwise                      = acc
```

Now, I can simply compare sizes to get my answer:
```hs
partOne :: Input -> Output
partOne input = M.size input - (S.size . getUnremovableBricks) input
```

## Okay, now let's make them fall!:

Now that we know which bricks we shouldn't remove, as they would make other bricks fall, let's remove them to make other bricks fall! :3c

First of all, I want a way to remove a brick from my bricks:
 - Because I represent my bricks with a map, I can simply remove the elements which keys are in the set of bricks to remove
 - For all other bricks, I simply need to remove the removed bricks from the supporting bricks set

```hs
-- Remove a set of bricks from the bricks
removeBricks :: S.Set Int -> Input -> Input
removeBricks toRemove = M.map removeSupporting . (`M.withoutKeys` toRemove)
    where removeSupporting c = c { supporting=S.difference (supporting c) toRemove }
```

Now that I can remove bricks from the tower, I need to know which bricks will now be falling.

A brick is falling if:
 - No other brick is supporting it

So, I simply need to check that:
```hs
-- Get the bricks that are no longer being supported
getFalling :: Input -> S.Set Int
getFalling = M.keysSet . M.filter (S.null . supporting)
```

Finally, I need to remove the falling bricks until no other brick is falling. I do that using a fix point algorithm:
 - Start by removing a brick.
 - Until there are no falling bricks, remove all of the falling bricks from the tower

This last part may sound counter-intuitive: the brick is not being removed, it is simply falling.

The reason it works is because we don't actually need to care about where the brick is going to land. All we care about is that, for a short amount of time, this brick would not be supporting any other brick (which may cause them to fall as well)

Another way to think about it is by saying that removing a brick from the tower is actually marking it as "has fallen".

```hs
-- Remove a brick and let the chain reaction happen
disintegrate :: Input -> Int -> Input
disintegrate bricks removed = until (null . getFalling) removeFalling startState -- Fix-point iteration
    where toRemove          = S.singleton removed
          startState        = removeBricks toRemove bricks
          removeFalling     = removeBricks =<< getFalling
```

And now, to solve part two:
 - I get my set of bricks which removal would cause other bricks to fall
 - For each brick I get the resulting tower after all other bricks are done falling
 - Now I get the number of falling bricks by comparing the size of the tower by the size of the tower after all bricks have fallen (or by comparing with the number of bricks that have not been marked as fallen if you prefer to see it that way)
 - I sum all the results together, and I'm done!

```hs
partTwo :: Input -> Output
partTwo input = sum fallenCount
    where toRemove        = (S.toList . getUnremovableBricks) input
          disintegrations = parMap rseq (disintegrate input) toRemove
          totalBricks     = M.size input
          fallenCount     = map (subtract 1 . (totalBricks -) . M.size) disintegrations
```
