module Interpolation where

type Point = (Double, Double)

linearInterpolate :: Point -> Point -> Double -> Double
linearInterpolate (x0, y0) (x1, y1) x =
    y0 + (y1 - y0) * (x - x0) /(x1 - x0)

newtonInterpolate :: [Point] -> Double -> Double
newtonInterpolate points x =
    let
        coefs = calcNewtonCoeffs points
        xs = map fst points
    in
        evalNewton xs coefs x
   
   
calcNewtonCoeffs :: [Point] -> [Double]
calcNewtonCoeffs [] = []
calcNewtonCoeffs points =
    let
        (xs, ys) = unzip points
        
        calcLayers :: Int -> [Double] -> [[Double]]
        calcLayers _ [] = []
        calcLayers _ [_] = []
        calcLayers step currentYs =
            let
                yPairs = zip currentYs (drop 1 currentYs)
                xPairs = zip xs (drop step xs)
                pairs = zip yPairs xPairs
                nextYs = map (\((y_curr, y_next), (x_curr, x_next)) -> 
                    (y_next - y_curr)/(x_next - x_curr)) pairs
            in
                nextYs : calcLayers (step + 1) nextYs
        
        getCoeffs :: [Double] -> [[Double]] -> [Double]
        getCoeffs (y:_) layers = y : map safeHead layers
            where
                safeHead [] = 0.0
                safeHead (h:_) = h
        getCoeffs _ _ = []
        
    in
        getCoeffs ys (calcLayers 1 ys)

evalNewton :: [Double] -> [Double] -> Double -> Double
evalNewton [] _ _ = 0.0
evalNewton xs coeffs targetX = 
    sum $ zipWith (*) coeffs xsTerms
    where
        xsTerms = scanl (\acc xi -> acc * (targetX - xi)) 1.0 xs
