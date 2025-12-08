module Day08.Main (day08) where

import           AOC       (submit)
import           Data.List (sortBy)
import           Data.Map  ((!), (!?))
import qualified Data.Map  as M
import           Data.Ord  (Down (..), comparing)

-- A data structure representing the connected components of an
-- undirected graph, without storing any of the graph’s edges.
data ComponentMap k = ComponentMap
  { idMap   :: M.Map k Int
  , sizeMap :: M.Map Int Int
  } deriving (Show, Eq)

mergeComponents :: Ord k => ComponentMap k -> Int -> Int -> ComponentMap k
mergeComponents (ComponentMap mIds mSize) id1 id2 = ComponentMap mIds' mSize'
    where (idMerge, idVoid) =  (min id1 id2, max id1 id2)
          mIds'  = M.map (\x -> if x == idVoid then idMerge else x) mIds
          mSize' = M.insert idVoid 0
                 . M.adjust (\x -> x + (mSize ! idVoid)) idMerge
                 $ mSize

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

type Position = (Int, Int, Int)
type Input = [Position]
type Output = Int

parseInput :: String -> Input
parseInput = map (\x -> read $ "(" ++ x ++ ")") . lines

distance :: Position -> Position -> Int
distance (x1, y1, z1) (x2, y2, z2) = x * x + y * y + z * z
    where x = x1 - x2
          y = y1 - y2
          z = z1 - z2

closestPairs :: Input -> [(Position, Position)]
closestPairs boxes = sortBy (comparing $ uncurry distance)
                     [(b1, b2) | b1 <- boxes, b2 <- boxes, b1 < b2]

partOne :: Input -> Output
partOne = product . take 3 . sortBy (comparing Down) . M.elems . sizeMap
        . foldl insertEdge (ComponentMap M.empty M.empty)
        . take 1000 . closestPairs

partTwo :: Input -> Output
partTwo boxes = x1 * x2
    where numBoxes = length boxes
          ((x1, _, _), (x2, _, _)) = closestPairs boxes !! (n - 1)
          n = length
            . takeWhile (\m -> sizeMap m !? 0 /= Just numBoxes)
            . scanl insertEdge (ComponentMap M.empty M.empty)
            $ closestPairs boxes

day08 :: String -> String -> IO ()
day08 "parse" = print . parseInput
day08 "one"   = print . partOne . parseInput
day08 "two"   = print . partTwo . parseInput
day08 "sone"  = submit 2025 8 1 . show . partOne . parseInput
day08 "stwo"  = submit 2025 8 2 . show . partTwo . parseInput
day08 _       = error "Undefined part"
