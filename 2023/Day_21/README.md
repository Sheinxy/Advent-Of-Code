## Day 21

I don't even want to write something for this one.
Having two days in a row where the input has special properties that you need to find out (properties that are not necessarilly present in the examples) is just annoying in my opinion 😿

## The input:

Another grid problem, another use for [Data.Matrix](https://hackage.haskell.org/package/matrix-0.3.6.3/docs/Data-Matrix.html)

```hs
data Input = Input { start :: (Int, Int), grid :: Matrix Char } deriving (Show)

parseInput :: String -> Input
parseInput input = Input start grid
    where grid  = (fromLists . lines) input
          start = head [(i, j) | (i,  row) <- zip [1 .. ] (lines input), 
                                 (j, char) <- zip [1 .. ] row,
                                 char == 'S']
```

I simply split my input file by lines, and transform it into a matrix. To get the starting point, I get the coordinate of the "S" tile. Nothing fancy here.

### 😿 Stupid input property number one: The starting point is always at the center of the file.

## How to do part one in multiple iterations:

Part one is about finding reachable tiles within a given number of steps. This screamed "[BFS](https://en.wikipedia.org/wiki/Breadth-first_search)!" in my head.

So my first algorithm was simple: I had a queue of pairs of the form (step_number, position) for each tile I would visit.
Then when the element in my queue has the target step_number, I put it in a list.
When the first element is not the target step number, I simply get my neighbours and put them back in the queue before going to the next element of my queue.

This was a silly algorithm. It worked, but quite slowly (4 seconds). My first mistake was to separate elements of my queue by positions instead of separating them by step number.

In fact, by reworking my algorithm to use pairs of the form (step number, positionS) where positions is the set of tiles at the beginning of the step, I was able to:
 - Speed up my code from 4s to 0.4s
 - Remove entirely the notion of queue. I was just changing the state instead.

One last optimisation that I did is, in my opinion, quite interesting:
 - Let's say that I am currently on state B. My next state will be step C and my previous step will be state A.
 Because of the way moving works, every tile in step A will be also present in step C (basically we're moving backwards)
 Therefore, to get the number of tiles at a new state, I simply need to look at the tiles that do not involve moving backwards. I simply need to look at the tiles that weren't in the previous state, and add the size of the previous state to the new state's size to get the answer.

 In order to implement that, instead of returning my list of tiles, my BFS returns the list of intermediate results (the size of each tile set) which is computed through an accumulator.
 When computing the size of the current state, I simply need to add the second element in my accumulator (starting with 0 for the first two states) to get my actual result.

Once I'm done, I can reverse my accumulator and remove the first two elements to re-order the states by number of steps (0 step will now be at index 0, 1 step at index 1 etc.)

 ```hs
getReachable :: Int -> Input -> [Int]
getReachable n (Input start grid) = drop 2 . reverse . bfs (0, singleton start) empty $ [0, 0]
    where wrapAround (r, c) = ((r - 1) `mod` nrows grid + 1, (c - 1) `mod` ncols grid + 1)
          isInWall   pos    = '#' == grid ! pos
          bfs (step, poss) old acc | step == n = acc'
                                   | otherwise = bfs state' poss acc'
              where neighboursOf pos = S.fromList                           .
                                       filter (not . isInWall . wrapAround) .
                                       filter (`notMember` old)             $
                                       [pos + dp | dp <- [(-1, 0), (1, 0), (0, -1), (0, 1)]]
                    acc'             = (acc !! 1 + size poss) : acc
                    state'           = (step + 1, unions . S.map neighboursOf $ poss)
```

And voilà! This runs way faster!

This iterative process of optimising my solution was actually quite interesting, and made me not totally dislike today's puzzle (wait for when I'm going to talk about part 2 to understand why I dislike it)

It also explains why my getReachable function looks like it does (it explains why I didn't use iterative, why (step, poss) is a tuple but old and acc are two separate arguments etc etc.)

And now to get my actual answer, I simply get the last value after 64 steps:
```hs
partOne :: Input -> Output
partOne = last . getReachable 64
```

## Too many things to notice:

Alright, it's time to look at the input and notice:
 - S is at the center of the map
 - The middle row and column are empty of any wall, making sure that it is possible to reach the edge in 65 steps (side length / 2). This means that in 131 steps you are "back" at the center of a chunk.
 - 26501365 is 2023 * 100 * 131 + 65, which means that it is 65 modulo 131.
 - If you plot the (step, number of tiles) points on a graph, you get this:

```
|.......................................................................................................................................................
|.......................................................................................................................................................
|.....................................................................................................................................................XX
|...................................................................................................................................................XX..
|.................................................................................................................................................XX....
|...............................................................................................................................................XX......
|............................................................................................................................................XXX........
|..........................................................................................................................................XX...........
|........................................................................................................................................XX.............
|......................................................................................................................................XX...............
|....................................................................................................................................XX.................
|..................................................................................................................................XX...................
|...............................................................................................................................XXX.....................
|.............................................................................................................................XX........................
|...........................................................................................................................XX..........................
|........................................................................................................................XXX............................
|......................................................................................................................XX...............................
|...................................................................................................................XXX.................................
|.................................................................................................................XX....................................
|..............................................................................................................XXX......................................
|............................................................................................................XX.........................................
|.........................................................................................................XXX...........................................
|......................................................................................................XXX..............................................
|...................................................................................................XXX.................................................
|................................................................................................XXX....................................................
|.............................................................................................XXX.......................................................
|..........................................................................................XXX..........................................................
|......................................................................................XXXX.............................................................
|...................................................................................XXX.................................................................
|................................................................................XXX....................................................................
|............................................................................XXXX.......................................................................
|........................................................................XXXX...........................................................................
|....................................................................XXXX...............................................................................
|................................................................XXXX...................................................................................
|...........................................................XXXXX.......................................................................................
|......................................................XXXXX............................................................................................
|................................................XXXXXX.................................................................................................
|..........................................XXXXXX.......................................................................................................
|..................................XXXXXXXX.............................................................................................................
|........................XXXXXXXXXX.....................................................................................................................
|XXXXXXXXXXXXXXXXXXXXXXXX...............................................................................................................................
|-------------------------------------------------------------------------------------------------------------------------------------------------------
```

THIS IS JUST A QUADRATIC EQUATION... 😾

Well, it is not an exact quadratic equation, but it turns out that, thanks to the properties of the input, it is possible to find one that will be exact for every x that is 65 modulo 131!

In order to do that, I simply perform some simple maths:
 - First of all, I get three points that are 65 modulo 131. I choose 65, 196 and 327 (that is half side, half side + side, half side + 2 * side)
 - Next I apply the following maths:

```
Basically I want to find a b and c such that

y0 = ax0^2 + bx0 + c
y1 = ax1^2 + bx1 + c
y2 = ax2^2 + bx2 + c

So I have a matrix matY:
(y0)
(y1)
(y2)

and a matrix matX:

(x0^2 x0 1)
(x1^2 x1 1)
(x2^2 x2 1)

And I want my matrix coefs:
(a)
(b)
(c)

such that

matY = matX * coefs

so:

coefs = matX^-1 * matY

Now, I simply take the matrix targetX:
(26501365^2 26501365 1)

and I do targetX * coefs, which is just doing
a * 26501365^2 + b * 26501365 + c

so I have my result
and I round because computers suck at maths
```

(Explanations pasted from a discussion I had with a friend 😸)

Put in code, this gives me:
```hs
partTwo :: Input -> Output
partTwo input = result
    where rows     = nrows $ grid input
          -- Get the results at three points: (half height, half height + height, half height + 2 height)
          -- We will call them x1, x2, x3
          results  = getReachable ((5 * rows) `div` 2) input
          xs       = [i * rows + rows `div` 2 | i <- [0 .. 2]]
          ys       = [fromIntegral $ results !! x | x <- xs]

          {- Make two matrices: 
                - a 3x3 matrix where each row is [x ^ 2, x, 1] for each x in our three points
                - a 3x1 matrix where each row is the number of tiles reachable after x1, x2 and x3
          -}
          matX     = fromLists [map fromIntegral [x ^ 2, x, 1] | x <- xs]
          matY     = fromList 3 1 ys

          -- Solve matY = matX * coefs for unknown coefs
          Right iX = inverse matX
          coefs    = iX `multStd` matY

          -- Get the [x^2, x, 1] for the target step count
          target   = 26501365
          targetX  = fromList 1 3 [target ^ 2, target, 1]
          
          -- Do targetX * coefs and get the result
          result   = round $ (targetX `multStd` coefs) ! (1, 1)
```


Goodbye cruel puzzle!
