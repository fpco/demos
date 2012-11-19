module Main where

import Types
import Json

import System.Random

-- FOR PERFORMANCE TESTING
import WorldDefs
import Simulation
import Criterion.Main
import Control.Parallel.Strategies
-- END FOR PERFORMANCE TESTING

-- PERFORMANCE TESTS
instance NFData World

main = defaultMain
         [ bench "advanceWorld solarWorld: "  $ whnf (advanceWorld 0.1) $ solarWorld
         , bench "toTree gridWorld: "         $ whnf toTree             $ parts gridWorld
         , bench "calcAccels gridWorld: "     $ whnf calcAccels         $ parts gridWorld
         , bench "advanceWorld gridWorld: "   $ whnf (advanceWorld 0.1) $ gridWorld
         , bench "advanceWorld simpleWorld: " $ whnf (advanceWorld 0.1) $ simpleWorld
         , bench "advanceWorld world4: "      $ whnf (advanceWorld 0.1) $ world4
         ]
-- END PERFORMANCE TESTS

-- RANDOM TESTING
-- Testing the construction of Barnes Hut Tree

maxDepth :: Int
maxDepth = 1

maxMass :: Double
maxMass = 5.0

-- Generate random BH Tree
genTree :: IO BHTree
genTree = do
  gen <- newStdGen
  return $ makeTree maxDepth gen 0 600 0 600

makeTree :: Int -> StdGen -> Double -> Double -> Double -> Double -> BHTree
makeTree 0 gen minx maxx miny maxy = BHT (Box minx miny maxx maxy) ((minx+maxx)/2.0) ((miny+maxy)/2.0) m []
  where
    (m,_) = randomR (0.0,maxMass) gen
makeTree depth gen minx maxx miny maxy =
    if stop
    then makeTree 0 gen minx maxx miny maxy
    else BHT (Box minx miny maxx maxy) centerx centery summass subtrees
  where
    (r,gen') = randomR (0,maxDepth) gen
    stop = r > depth
    subtrees = map (makeTree' (depth-1)) (zip quads [g11,g12,g21,g22])
    cx = (minx+maxx)/2.0
    cy = (miny+maxy)/2.0
    (g1,g2) = split gen'
    ((g11,g12),(g21,g22)) = (split g1, split g2)
    quads =
      [ (minx,miny,cx,cy)
      , (cx,miny,maxx,cy)
      , (minx,cy,cx,maxy)
      , (cx,cy,maxx,maxy)
      ]
    makeTree' depth ((minx,miny,maxx,maxy), g) = makeTree depth g minx maxx miny maxy
    centerx = sum [xx*m | BHT _ xx _ m _ <- subtrees] / summass
    centery = sum [yy*m | BHT _ _ yy m _ <- subtrees] / summass
    summass = sum [m    | BHT _ _ _  m _ <- subtrees]

