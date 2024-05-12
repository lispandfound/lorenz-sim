{-# LANGUAGE TemplateHaskell, DataKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
module Main where


import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD
import Diagrams.ThreeD.Projection
import Diagrams.LinearMap
import Linear.V3
import Linear.V2
import Linear.Vector
import Linear.Metric (dot)

sigma :: Double
sigma = 10

rho :: Double
rho = 28

beta :: Double
beta = 8/3

dt :: Double 
dt = 0.001

numPoints :: Int
numPoints = 50000

colourLerp :: Int -> Int -> Colour Double -> Colour Double -> Colour Double
colourLerp step steps a b = blend (fromIntegral step / fromIntegral steps) a b

lorenzDx :: V3 Double -> V3 Double
lorenzDx (V3 x y z) = V3 (sigma * (y - x)) (x * (rho - z) - y) (x * y - beta * z)

euler :: (V3 Double -> V3 Double) -> Double -> V3 Double -> [V3 Double]
euler fdx dt x0 = iterate ((+) <*> ((dt *^) . fdx)) x0

perspectiveView :: AffineMap V3 V2 Double
perspectiveView = lookingAt (p3 (30, -30, 60)) (p3 (0, 0, 0)) (direction (V3 0 0 1))


lorenzPoints :: [Point V2 Double]
lorenzPoints = map (amap perspectiveView . P) $ euler lorenzDx dt (V3 2 1 1)

lorenzLine :: Trail' Line V2 Double
lorenzLine =  fromVertices . take numPoints $ lorenzPoints

lorenzTrajectory :: Trail V2 Double
lorenzTrajectory = Trail lorenzLine

lorenzDiagram :: Diagram B
lorenzDiagram = pad 1.1 .  centerXY . mconcat . zipWith (\i segment -> lc (colourLerp i numPoints purple pink) segment) [0..] . explodeTrail $ (lorenzTrajectory `at` origin)

main :: IO ()
main = mainWith lorenzDiagram
