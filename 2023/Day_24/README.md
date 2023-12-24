## Day 24

Merry Christmas eve everyone!

Today was my first time using a SAT solver :D

## The input:

I represent my hailstones with a data structure containing their positions and velocitys as 3-tuples:
```hs
data Hailstone = Hailstone { position :: (Int, Int, Int), velocity :: (Int, Int, Int) } deriving (Show, Eq, Ord)
```

I make this data structure an instance of Ord for a nice trick that I will explain later :)

To parse the input, I simply get the three first and last words of each line, transform them into 3-tuples of Int and put them in my data structure:
```hs
type Input = [Hailstone]

parseInput :: String -> Input
parseInput = map (makeHail . getVals) . lines
    where getVals line    = map (read . ('(' :) . ( ++ ")") . concat) [ take 3 $ words line, drop 4 $ words line]
          makeHail [a, b] = Hailstone a b
```

## Finding where 2 lines intersect:

Finding where 2 lines intersect is quite easy. I used the [following formula](https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line_segment) which takes to lines represented by two points and returns the intersection point of these two lines.

The way this formula works is by working with "time equations" for each line. Basically, each line is represented by an equation:
```
L = initial point + t * step
```
where t is the time.

Here, our initial point is the position of a hailstone, and the step is the velocity.

However, what we want to work on isn't directly the velocity, but another point. The step can then be found by taking the difference between the initial point and the new point.

So I start by adding my velocity to my starting point to get the next position after 1ms. My t (or u for the second hailstone) parameter will therefore represent the time in nanoseconds.

When computing the values for t and u (moments in time when the hailstone will arrive at the intersection point), I can easily find if the hailstones would intersect in the past (simply by look at if t or u is negative). In that case, I return Nothing (i.e the hailstones won't cross paths)

If the denominator in my formula is 0, t and u would be +- infinity, meaning the two paths are parallel and won't cross, I also return nothing.

In any other case, I simply compute my intersection point with
```
initial_pos1 + t * velocity1
```

```hs
find2DIntersection :: Hailstone -> Hailstone -> Maybe (Double, Double)
find2DIntersection h1 h2 | denominator == 0 || t < 0 || u < 0 = Nothing
                         | otherwise                          = Just intersectP
                         where (x1, y1, _) = position h1
                               (x2, y2, _) = position h1 + velocity h1
                               (x3, y3, _) = position h2
                               (x4, y4, _) = position h2 + velocity h2
                               denominator = fromIntegral $ (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
                               numT        = fromIntegral $ (x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)
                               numU        = fromIntegral $ (x1 - x3) * (y1 - y2) - (y1 - y3) * (x1 - x2)
                               (t, u)      = (numT / denominator, numU / denominator)
                               intersectP  = (fromIntegral x1 + t * fromIntegral (x2 - x1), fromIntegral y1 + t * fromIntegral (y2 - y1))
```

Now, to solve part 1, I simply need to find the intersection of each unique hailstone pair, keep the ones that don't return Nothing, keep those in the testing area and count their numbers.

To only get unique hailstone pairs, I simply iterate over each **ORDERED** hailstone pair (h1, h2) where h1 < h2 (this is the nice trick I teased about earlier :)):

```hs
getIntersectingInArea :: Double -> Double -> Input -> Output
getIntersectingInArea lo hi input = length . filter isInArea $ intersections
    where intersections = catMaybes [find2DIntersection h1 h2 | h1 <- input, h2 <- input, h1 < h2]
          isInArea      = uncurry (&&) . both (\x -> lo <= x && x <= hi)
```

[catMaybes](https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Maybe.html#v:catMaybes) is a really nice function that takes a list of maybes and returns the list of all Just values!

```hs
partOne :: Input -> Output
partOne = getIntersectingInArea 200000000000000 400000000000000
```

## SAT Solving because I'm lazy :3c :

Remember when I said a few days ago that I don't like problems involving 3 dimensions because I'm bad at visualising them?

Well this problem is in 4 dimensions, so it's worse! :,D

Because of this, I started by trying to solve it for a more simple case:
 - I wanted to see how I could solve this only caring about the x axis

After some trial and error, I realised that I was slowly defining constraints, with a bunch of unknowns, that I wanted to satisfy.

What I was slowly getting at, without even realising it, was [constraint programming](https://en.wikipedia.org/wiki/Constraint_programming)

Once I noticed that, I immediately thought about using [z3](https://en.wikipedia.org/wiki/Z3_Theorem_Prover) to get my answer.

If you don't know z3 (or any other SMT/SAT solvers), basically: it is a tools that you give constraints equations to and that can check if these equations are solvable and even solve them.

I quickly tried to make a proof of concept in Python (as I have already seen python code using z3 in my life, it felt easier to first try in python):

![my poc](https://media.discordapp.net/attachments/905061047759667221/1188438461687021598/2023-12-24-120957_613x223_scrot.png?ex=659a8691&is=65881191&hm=78293c24ca9a880f03ffb6c53046c97511d52aef88eda6a41abe2112ee979fe8&=&format=webp&quality=lossless)

My idea here is to basically have:
 - 6 variables (the position and velocity of my rock)
 - 300 other variables (the time of intersection with each hailstone in the input)
 - 3 equations and one inequality per hailstone (hailstone i intersects with the rock at time ti, and time ti is strictly positive)

This could, in fact, be reduced to 9 variables and 12 equations, and I only need to find the possible answers for three hailstones and check which one works with every other hailstones. This would be faster, but I can't be bothered doing it.

In order to use z3, I am using the [sbv library](https://hackage.haskell.org/package/sbv-10.2/docs/Data-SBV.html):

```hs
satSolve :: Input -> IO SatResult
satSolve input = sat $ do
    -- Technically this should be sIntegers, however sReals is faster and the input is specific enough to yield integers
    [x, y, z, vx, vy, vz] <- sReals ["x", "y", "z", "vx", "vy", "vz"]

    for_ (zip [1 .. ] input) $ \(i, Hailstone (hx, hy, hz) (hvx, hvy, hvz)) -> do
        t <- sReal ("t" ++ show i)
        constrain $ t .> 0
        constrain $ fromIntegral hx + t * fromIntegral hvx .== x + t * vx 
        constrain $ fromIntegral hy + t * fromIntegral hvy .== y + t * vy 
        constrain $ fromIntegral hz + t * fromIntegral hvz .== z + t * vz 
```

With that, I now only need to get back the x, y, z values and add them together. A funny thing though is that I am working on sReals (which gives me the right answer because of some properties of the input I believe. The reason I am not using sIntegers is because it was too slow on my input) which means that I get an AlgReal result where I want an Int result.

To convert my AlgReal value to an Int, I do something weird:
 - I convert it to a string
 - I take only the part before the decimal point
 - I convert it to an int

```hs
partTwo :: Input -> IO Output
partTwo input = do
    res  <- satSolve input
    let x = fromJust $ "x" `getModelValue` res
    let y = fromJust $ "y" `getModelValue` res
    let z = fromJust $ "z" `getModelValue` res
    let s = (x + y + z) :: AlgReal
    return $ read . takeWhile (/= '.') . show $ s -- Funky conversion from AlgReal to Int :)
```

I am not quite knowledgeable with Monads and the do notation for now, this is mostly my first time using the this much. Something to learn more about!
