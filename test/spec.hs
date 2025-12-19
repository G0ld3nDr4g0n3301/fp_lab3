module Main where

import Test.Hspec
import Interpolation
import Processor


linearAdapter :: [(Double, Double)] -> Double -> Double
linearAdapter (p1:p2:_) x = linearInterpolate p1 p2 x
linearAdapter _ _ = 0/0

main :: IO ()
main = hspec $ do
    describe "Interpolation module" $ do
        describe "linearInterpolate" $ do
            it "interpolates correctly" $ do
                linearInterpolate (0, 0) (2, 4) 1 `shouldBe` 2
                linearInterpolate (1, 1) (3, 9) 2 `shouldBe` 5
            
            it "returns exact values at endpoints" $ do
                linearInterpolate (0, 0) (2, 4) 0 `shouldBe` 0
                linearInterpolate (0, 0) (2, 4) 2 `shouldBe` 4
        
        describe "newtonInterpolate" $ do
            it "interpolates 2 points correctly" $ do
                newtonInterpolate [(0,0), (1,1)] 0.5 `shouldBe` 0.5
            
            it "interpolates 3 points correctly (quadratic)" $ do
                newtonInterpolate [(0,0), (1,1), (2,4)] 0.5 `shouldBe` 0.25
                newtonInterpolate [(0,0), (1,1), (2,4)] 1.5 `shouldBe` 2.25
            
            it "returns exact values at data points" $ do
                let points = [(0,0), (1,1), (2,4), (3,9)]
                newtonInterpolate points 0 `shouldBe` 0
                newtonInterpolate points 1 `shouldBe` 1
                newtonInterpolate points 2 `shouldBe` 4
                newtonInterpolate points 3 `shouldBe` 9
    
    describe "Processor module" $ do
        
        describe "runSlidingWindow" $ do
            let linearConfig = AlgoConfig "linear" 2 0.5 linearAdapter
            
            it "returns empty for insufficient points" $ do
                runSlidingWindow linearConfig [(0,0)] `shouldBe` []
            
            it "processes basic case" $ do
                let result = runSlidingWindow linearConfig [(0,0), (1,1), (2,4)]
                let expected = [("linear", (0.0, 0.0)), 
                               ("linear", (0.5, 0.5)),
                               ("linear", (1.0, 1.0)),
                               ("linear", (1.5, 2.5)),
                               ("linear", (2.0, 4.0))]
                result `shouldBe` expected
            
            it "handles two-point input correctly" $ do
                let result = runSlidingWindow linearConfig [(0,0), (1,1)]
                let expected = [("linear", (0.0, 0.0)),
                               ("linear", (0.5, 0.5)),
                               ("linear", (1.0, 1.0))]
                result `shouldBe` expected
            
            it "handles four-point input correctly" $ do
                let points = [(0,0), (1,1), (2,4), (3,9)]
                let result = runSlidingWindow linearConfig points
                length result `shouldBe` 7
                last result `shouldBe` ("linear", (3.0, 9.0))
                result !! 1 `shouldBe` ("linear", (0.5, 0.5))
                result !! 3 `shouldBe` ("linear", (1.5, 2.5))
                result !! 5 `shouldBe` ("linear", (2.5, 6.5))
            
            it "does not have duplicate x-values" $ do
                let points = [(0,0), (1,1), (2,4), (3,9)]
                let result = runSlidingWindow linearConfig points
                let xs = map (\(_, (x, _)) -> x) result
                -- Проверяем, что все x уникальны
                xs `shouldBe` nub xs
                where
                    nub [] = []
                    nub (x:xs) = x : nub (filter (/= x) xs)