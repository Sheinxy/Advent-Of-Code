module AOC.Utils where

-- Iterate until a fixed-point is found
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f x = let x' = f x in
    if x == x' then x
    else fixedPoint f x'

-- Is a between b and c included
between :: Ord a => a -> a -> a -> Bool
between a b c = b <= a && a <= c

-- Is a between b and c excluded
between' :: Ord a => a -> a -> a -> Bool
between' a b c = b < a && a < c

-- Is a in a closed range
isInRange :: Ord a => a -> (a, a) -> Bool
isInRange a (b, c)= between a b c

-- Is a in an open range
isInRange' :: Ord a => a -> (a, a) -> Bool
isInRange' a (b, c) = between' a b c

-- Similar to groupBy but without the transitive property
-- Each element in the sublist matches the predicate with at
-- least one other element before it in the list.
groupByNT :: (a -> a -> Bool) -> [a] -> [[a]]
groupByNT predicate = groupByNT' []
    where groupByNT' acc [] = [reverse acc]
          groupByNT' [] (x : xs) = groupByNT' [x] xs
          groupByNT' acc l@(x : xs) | any (`predicate` x) acc = groupByNT' (x : acc) xs
                                    | otherwise = (reverse acc) : groupByNT' [] l
                                
