module Day09.Main (day09) where

import           AOC       (submit)
import           AOC.Utils (isInRange)

type Point = (Int, Int)
type Segment = (Point, Point)
type Polygon = [Segment]

type Input = [Point]
type Output = Int

parseInput :: String -> Input
parseInput = map (\x -> read $ "(" ++ x ++ ")") . lines

area :: (Int, Int) -> (Int, Int) -> Int
area (a, b) (c, d) = (abs (a - c) + 1) * (abs (b - d) + 1)

partOne :: Input -> Output
partOne tiles = maximum [area x y | x <- tiles, y <- tiles, x < y]

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

intersectsPolygon :: Polygon -> Segment -> Bool
intersectsPolygon poly segment = any (edgeIntersects segment) poly

partTwo :: Input -> Output
partTwo tiles = maximum
              . map (uncurry area)
              . filter (not . any (intersectsPolygon loop) . getRectangleEdges)
              $ rects
    where rects = [(x, y) | x <- tiles, y <- tiles, x < y]
          loop  = getLoop tiles

day09 :: String -> String -> IO ()
day09 "parse" = print . parseInput
day09 "one"   = print . partOne . parseInput
day09 "two"   = print . partTwo . parseInput
day09 "sone"  = submit 2025 9 1 . show . partOne . parseInput
day09 "stwo"  = submit 2025 9 2 . show . partTwo . parseInput
day09 _       = error "Undefined part"
