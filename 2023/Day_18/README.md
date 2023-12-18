## Day 18

Today wasn't too hard! However, it's quite late right now (I was celebrating my brother's birthday!), so I will make this write-up as short and quick as possible (sorry 😸)

## The input:

For today, the input is a list of instructions, where the instructions are: A direction, a number of steps, a colour.

So this is how I get my input:

```hs
type Input = [(String, Int, String)]

parseInput :: String -> Input
parseInput = map (go . words) . lines
    where go [dir, dist, _:colour] = (dir, read dist, init colour)
```

I split by lines. The I split each line by spaces, and I get the value of each chunk. I store each line as a 3-tuple (Direction, Steps, Colour)

## [I am a dwarf and I'm digging the hole](https://www.youtube.com/watch?v=ytWz0qVvBZ0):

This list of instructions is nice, but to actually compute anything what I really want to do is to get the generated tranches.

Digging the tranches acutally form a polygon. I can therefore represent my tranches as a list of vertices (the vertices of the polygons, the "corners"):

```hs
digTranches :: Input -> [Vertex]
digTranches = scanl dig (0, 0)
    where dig (r, c) ("L", n, _) = (r    , c - n)
          dig (r, c) ("R", n, _) = (r    , c + n)
          dig (r, c) ("U", n, _) = (r - n, c    )
          dig (r, c) ("D", n, _) = (r + n, c    )
```

I start at point (0, 0), and I move to the next corner by following my instructions, and I keep in memory every vertex that I've encountered.

If the instructions set is well defined, then my first vertex and my last vertex are equal. This is not a problem (and in fact this is something that will come in handy later!)

## [The tip of the shoelace formula](https://www.youtube.com/watch?v=r_DuY0CQUz4):

Now, what we want to know is the number of squares this shape has. My first idea was to simply compute the area of the polygon using the [shoelace formula](https://en.wikipedia.org/wiki/Shoelace_formula) (well, I prefer the triangle formula tbh, I find it simpler to describe. But they're basically the same formula)

However, this would not exactly work for some simple reasons:
 - First of all, we need to include the border of our polygon in our final result
 - The polygon formed by our vertices is actually not the polygon we want the area of

 For that last point, let's look at the example:
 ```
#######
#.....#
###...#
..#...#
..#...#
###.###
#...#..
##..###
.#....#
.######
```

The vertices we have are all located on the top-left corner of each tile, which means that our area is going to include some parts that we don't want! (for example, it will include the whole top-left tile)

To solve that, we could simply modify our digTranches function to get the outside corner of each tile instead (and therefore we would simply need to compute the area to get our answer). However there is another possibility:

Instead of looking at the area itself, we need to count the number of integer points inside the area. This can be done using [Pick's theorem](https://en.wikipedia.org/wiki/Pick's_theorem)

Here, the area of the polygon is the one we compute using the shoelace formula. The number of inside points is the number we want, and the number of bordering points is our perimeter.

Rewriting pick's theorem, we get i = A - b/2 + 1

Now, we don't just want the inside points. We also want the bordering points in our final answer:

answer = i + b = A - b/2 + 1 = A + b/2 + 1

This gives us:

```hs
area :: [Vertex] -> Int
area vertices = 1 + perimeter `div` 2 + (abs . (`div` 2) . sum . zipWith crossProduct vertices $ tail vertices)
    where crossProduct (r1, c1) (r2, c2) = c1 * r2 - r1 * c2
          dist         (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)
          perimeter = sum . zipWith dist vertices $ tail vertices
```

Thanks for reading, i am now going to sleep <3
