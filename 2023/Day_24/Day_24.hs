module Main where

import Data.List
import Data.Maybe
import Data.NumInstances.Tuple
import Data.Foldable
import Data.Tuple.Extra (both)
import Data.SBV
import System.Environment

data Hailstone = Hailstone { position :: (Int, Int, Int), velocity :: (Int, Int, Int) } deriving (Show, Eq, Ord)

type Input = [Hailstone]
type Output = Int

parseInput :: String -> Input
parseInput = map (makeHail . getVals) . lines
    where getVals line    = map (read . ('(' :) . ( ++ ")") . concat) [ take 3 $ words line, drop 4 $ words line]
          makeHail [a, b] = Hailstone a b

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

getIntersectingInArea :: Double -> Double -> Input -> Output
getIntersectingInArea lo hi input = length . filter isInArea $ intersections
    where intersections = catMaybes [find2DIntersection h1 h2 | h1 <- input, h2 <- input, h1 < h2]
          isInArea      = uncurry (&&) . both (\x -> lo <= x && x <= hi)

partOne :: Input -> Output
partOne = getIntersectingInArea 200000000000000 400000000000000

satSolve :: Input -> IO SatResult
satSolve input = satWith z3 $ do
    -- Technically this should be sIntegers, however sReals is faster and the input is specific enough to yield integers
    [x, y, z, vx, vy, vz] <- sReals ["x", "y", "z", "vx", "vy", "vz"]

    for_ (zip [1 .. ] input) $ \(i, Hailstone (hx, hy, hz) (hvx, hvy, hvz)) -> do
        t <- sReal ("t" ++ show i)
        constrain $ t .> 0
        constrain $ fromIntegral hx + t * fromIntegral hvx .== x + t * vx 
        constrain $ fromIntegral hy + t * fromIntegral hvy .== y + t * vy 
        constrain $ fromIntegral hz + t * fromIntegral hvz .== z + t * vz 

partTwo :: Input -> IO Output
partTwo input = do
    res  <- satSolve input
    let x = fromJust $ "x" `getModelValue` res
    let y = fromJust $ "y" `getModelValue` res
    let z = fromJust $ "z" `getModelValue` res
    let s = (x + y + z) :: AlgReal
    return $ read . takeWhile (/= '.') . show $ s -- Funky conversion from AlgReal to Int :)

compute :: Input -> String -> IO ()
compute input "parse" = print input
compute input "one"   = print . partOne $ input
compute input "two"   = print =<< partTwo input
compute input _       = error "Unknown part"

main = do
    args  <- getArgs
    input <- parseInput <$> readFile (last args)
    mapM (compute input) $  init args 
