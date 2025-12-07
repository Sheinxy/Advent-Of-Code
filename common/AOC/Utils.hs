module AOC.Utils where

-- Iterates until a fixed-point is found
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f x = let x' = f x in
    if x == x' then x
    else fixedPoint f x'

-- Is a between b and c included ?
between :: Ord a => a -> a -> a -> Bool
between a b c = b <= a && a <= c

-- Is a between b and c excluded ?
between' :: Ord a => a -> a -> a -> Bool
between' a b c = b < a && a < c

-- Is a in a closed range ?
isInRange :: Ord a => a -> (a, a) -> Bool
isInRange a (b, c)= between a b c

-- Is a in an open range ?
isInRange' :: Ord a => a -> (a, a) -> Bool
isInRange' a (b, c) = between' a b c

-- Groups consecutive elements such that each element in a group satisfies
-- the predicate with at least one earlier element in the same group.
connectBy :: (a -> a -> Bool) -> [a] -> [[a]]
connectBy _ [] = []
connectBy predicate (x : xs) = go [x] xs
    where go acc [] = [reverse acc]
          go acc (y : ys) | any (`predicate` y) acc = go (y : acc) ys
                          | otherwise = reverse acc : go [y] ys

-- Takes a 2D list and returns an indexed 1D list.
index2D :: [[a]] -> [((Int, Int), a)]
index2D g = [((i, j), x) | (i, row) <- zip [0..] g, (j, x) <- zip [0..] row]
