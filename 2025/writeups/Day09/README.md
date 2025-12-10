## Day 09

I need to sleep.

## The Input

The input is just a list of points.

We covered that [yesterday](../Day08) already (wait, I'm actually late so it was the day before yesterday).

Haskell can parse it, let it parse :)

```hs
type Input = [Point]

parseInput :: String -> Input
parseInput = map (\x -> read $ "(" ++ x ++ ")") . lines
```

## Part One: the easy part

### The Problem

We need to find the biggest area we can get by making rectangles
were the opposite corners are two points from the list.

### The Solution

Let's start by making sure we know how to compute the area. It's harder
than you think because integers are funky :) (i.e don't forget the +1)

```hs
type Point = (Int, Int)

area :: Point -> Point -> Int
area (a, b) (c, d) = (abs (a - c) + 1) * (abs (b - d) + 1)
```

Now, let's just get all possible rectangles and find the maximum area :D

```hs
type Output = Int

partOne :: Input -> Output
partOne tiles = maximum [area x y | x <- tiles, y <- tiles, x < y]
```

That's it :D

This works by just getting all the ordered pairs and computing their area.

## Part Two: the easy part that is hard

### The Problem

The input forms a closed loop. Find the biggest rectangle you can make, like part one,
but make sure it fits inside the loop :)

### The Solution

The solution is pretty simple: if any of the rectangle's edges crosses the edge of
the loop, then it can't fit inside. Otherwise it is fully inside (we know that because
at least two points are inside the loop, as they're on its border).

The problem here is handling the fact that we will get intersection points on the loop's
vertices, as they are also part of the rectangle. Furthermore, in this case it is
quite hard to know if this intersection is fine or if the edge crosses the loop.

The solution is easy: don't take the outer edge of the rectangle, but it's inner edge
(we're working with integer points here, so everything has some thickness!)

Let's start by getting the loop and the edges of a rectangle:
```hs
type Segment = (Point, Point)
type Polygon = [Segment]

getLoop :: Input -> Polygon
getLoop tiles = zip tiles $ tail (cycle tiles)

getRectangleEdges :: (Point, Point) -> Polygon
getRectangleEdges ((x1, y1), (x2, y2)) = [(a, b)
                                         | a@(x , y ) <- corners
                                         , b@(x', y') <- corners
                                         , a < b, x == x' || y == y'
                                         ]
    where corners = [(minX, minY), (maxX, minY), (minX, maxY), (maxX, maxY)]
          (minX, maxX) = (min x1 x2 + 1, max x1 x2 - 1)
          (minY, maxY) = (min y1 y2 + 1, max y1 y2 - 1)
```

We get the loop by zipping each tile with the next one in the list, cycling back to the first one at the end.

The rectangle's edges are just the pairs of corners that are either on the same line or column.
(pretty sure this doesn't work for rectangles where the two opposite corners are on the same line or column, but they won't be the answer anyway so dnc)

Now, I'm using the (overkilled considering we're working with integers) famous [line intersection formula](https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line)
to find if two segments intersect:

```hs
edgeIntersects :: Segment -> Segment -> Bool
edgeIntersects ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4))
        | den == 0 = False
        | otherwise =  isInRange px (min x1 x2, max x1 x2) && isInRange py (min y1 y2, max y1 y2)
                    && isInRange px (min x3 x4, max x3 x4) && isInRange py (min y3 y4, max y3 y4)
    where den = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
          numPx = (x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4)
          numPy = (x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4)
          px = numPx `div` den
          py = numPy `div` den
```

In order to check if a segment intersect with the loop, we can simply check against all its edges:

```hs
intersectsPolygon :: Polygon -> Segment -> Bool
intersectsPolygon poly segment = any (edgeIntersects segment) poly
```

Now, we just need to filter out the rectangles whose edges intersect with the loop:
```hs
partTwo :: Input -> Output
partTwo tiles = maximum
              . map (uncurry area)
              . filter (not . any (intersectsPolygon loop) . getRectangleEdges)
              $ rects
    where rects = [(x, y) | x <- tiles, y <- tiles, x < y]
          loop  = getLoop tiles
```

## Conclusion

[Bye Bye](https://www.youtube.com/watch?v=UkEFmv171MM)
