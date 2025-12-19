module Processor where

import Interpolation (Point)

type Interpolator = [Point] -> Double -> Double

data AlgoConfig = AlgoConfig
  { algoName :: String, -- Имя алгоритма("newton", "linear")
    windowSize :: Int, -- Размер окна
    step :: Double, -- шаг дискретизации
    interpolator :: Interpolator
  }

safeHead :: [Point] -> Point
safeHead [] = (0.0, 0.0)
safeHead (x : _) = x

generateXs :: Double -> Double -> Double -> Bool -> [Double]
generateXs start end genStep isLastSegment =
  let epsilon = 1e-9
      limit = if isLastSegment then end + epsilon else end - epsilon
   in takeWhile (< limit) [start, start + genStep ..]

runSlidingWindow :: AlgoConfig -> [Point] -> [(String, Point)]
runSlidingWindow config inputStream =
  let n = windowSize config
      (initialWindow, restStream) = splitAt n inputStream
   in if length initialWindow < n
        then []
        else
          let mid = n `div` 2

              firstStart = fst (safeHead initialWindow)
              firstEnd = fst (initialWindow !! mid)

              firstChunk = calcSegment config initialWindow firstStart firstEnd False
           in firstChunk ++ loop mid initialWindow restStream
  where
    loop :: Int -> [Point] -> [Point] -> [(String, Point)]
    loop mid window [] =
      let start = fst (window !! mid)
          end = fst (last window)
       in calcSegment config window start end True
    loop mid window (newPoint : rest) =
      let newWindow = drop 1 window ++ [newPoint]
          start = fst (window !! mid)
          end = fst (newWindow !! mid)
       in calcSegment config newWindow start end False ++ loop mid newWindow rest

    calcSegment :: AlgoConfig -> [Point] -> Double -> Double -> Bool -> [(String, Point)]
    calcSegment conf window startX endX isLast =
      let xs = generateXs startX endX (step conf) isLast
          calcY = interpolator conf window
       in map (\x -> (algoName conf, (x, calcY x))) xs
