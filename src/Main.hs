 module Main where

import System.Environment (getArgs)
import System.IO (stdout)
import Text.Printf (printf)
import Data.Maybe (catMaybes)

import Interpolation (Point, linearInterpolate, newtonInterpolate)
import Processor (AlgoConfig(..), runSlidingWindow)


data AppConfig = AppConfig
    { useLinear :: Bool
    , useNewton :: Bool
    , stepVal   :: Double
    , newtonN   :: Int
    } deriving Show

defaultConfig :: AppConfig
defaultConfig = AppConfig False False 1.0 4

parseArgs :: [String] -> AppConfig -> AppConfig
parseArgs [] conf = conf
parseArgs ("--linear":xs) conf = parseArgs xs (conf { useLinear = True })
parseArgs ("--newton":xs) conf = parseArgs xs (conf { useNewton = True })
parseArgs ("--step":val:xs) conf = parseArgs xs (conf { stepVal = read val })
parseArgs ("-n":val:xs) conf = parseArgs xs (conf { newtonN = read val })
parseArgs (_:xs) conf = parseArgs xs conf -- игнорируем неизвестные флаги

parsePoint :: String -> Maybe Point
parsePoint line =
    let 
        cleanLine = map (\c -> if c == ';' || c == '\t' then ' ' else c) line
        tokens = words cleanLine
    in case tokens of
        [xStr, yStr] -> Just (read xStr, read yStr)
        [xStr, yStr, _] -> Just (read xStr, read yStr)
        _ -> Nothing

mergeStreams :: [(String, Point)] -> [(String, Point)] -> [(String, Point)]
mergeStreams [] ys = ys
mergeStreams xs [] = xs
mergeStreams ax@((tagA, (xa, ya)):xs) ay@((tagB, (xb, yb)):ys)
    | xa <= xb  = (tagA, (xa, ya)) : mergeStreams xs ay
    | otherwise = (tagB, (xb, yb)) : mergeStreams ax ys

linearAdapter :: [Point] -> Double -> Double
linearAdapter (p1:p2:_) x = linearInterpolate p1 p2 x
linearAdapter _ _ = 0/0

newtonAdapter :: [Point] -> Double -> Double
newtonAdapter = newtonInterpolate

main :: IO ()
main = do

    args <- getArgs
    let conf = parseArgs args defaultConfig

    if not (useLinear conf) && not (useNewton conf)
        then putStrLn "Error: Please specify at least one algorithm: --linear or --newton"
        else do
            content <- getContents
            let inputLines = lines content
            let validLines = filter (not . null) inputLines
            let maybePoints = map parsePoint validLines
            let points = catMaybes maybePoints
            
            let linearStream = if useLinear conf
                               then runSlidingWindow (AlgoConfig "linear" 2 (stepVal conf) linearAdapter) points
                               else []

            let newtonStream = if useNewton conf
                               then runSlidingWindow (AlgoConfig "newton" (newtonN conf) (stepVal conf) newtonAdapter) points
                               else []

            let finalOutput = mergeStreams linearStream newtonStream

            mapM_ printResult finalOutput

printResult :: (String, Point) -> IO ()
printResult (algo, (x, y)) = printf "%s: %.4f %.4f\n" algo x y