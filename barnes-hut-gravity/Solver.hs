{-# LANGUAGE BangPatterns, PatternGuards #-}

-- | The list version of the solver also builds the bounding box at every
--   node of the tree, which is good for visualisation.
module Solver(calcAccels, toTree, toSet, tparts) where

import Types
import Constants
import Control.Parallel.Strategies

tparts = [ Particle (Mass sunMass) (Pos 0 0) (Vel 0 0)
        , Particle (Mass earthMass) (Pos 144173309583.76483 49415212933.30807) (Vel 0 earthVelocity)
        ]
  where
    sunMass         = 1.9891e30
    earthDistance   = 152098232e3   -- Aphelion
    earthMass       = 5.9736e24
    earthVelocity   = 29.78e3


-- | Compute the acclerations on all these points.
calcAccels :: [Particle] -> ([Accel], BHTree)
calcAccels mpts = (accels, bhtree)
  where bhtree = toTree mpts
        accels = parMap rseq (flip calcAccel bhtree) mpts

-- A small number
small :: Double
small = 10

-- Convert a Barnes Hut Tree to a set of particles
toSet :: BHTree -> [Particle]
toSet (BHT (Box minx miny maxx maxy) x y m []) = [Particle (Mass m) (Pos x y) (Vel 0 0)]
toSet (BHT _ _ _ _ st) = concatMap toSet st

-- Convert a Set of particles to a Barnes Hut Tree
toTree :: [Particle] -> BHTree
toTree ps = toTreeWithBox bounds ps
  where
    -- small is subtracted from the initial left and lower bounds
    --   to ensure that points on the boundary are included
    bounds = Box (minimum xx - small) (minimum yy - small) (maximum xx) (maximum yy)
    (xx, yy) = unzip [(x,y) | Particle _ (Pos x y) _ <- ps]

-- Convert a set of particles within a bounding box to a Barnes Hut Tree
toTreeWithBox :: BoundingBox -> [Particle] -> BHTree
toTreeWithBox bb [] = BHT bb 0 0 0 []
toTreeWithBox bb [Particle (Mass m) (Pos x y) _] = BHT bb x y m []
toTreeWithBox bb@(Box minx miny maxx maxy) ps = BHT bb x y m st
  where

    -- Compute the centroid of the particles
    tsum (x1,y1,m1) (x2,y2,m2) = (x1+x2, y1+y2, m1+m2)
    (x',y',m) = foldl1 tsum [(x*m, y*m, m) | BHT _ x y m _ <- st]
    x = x' / m
    y = y' / m

    -- Split the parent bounding box into four quadrants.
    cx = (minx+maxx)/2.0
    cy = (miny+maxy)/2.0
    quads =
      [ Box minx miny cx cy
      , Box cx miny maxx cy
      , Box minx cy cx maxy
      , Box cx cy maxx maxy
      ]

    -- Recursively compute the subtrees
    st = map toTreeWithBox' quads
    toTreeWithBox' box = toTreeWithBox box [p | p <- ps, withinBox box p]

-- | Check if a particle is in box (excluding left and lower border)
withinBox :: BoundingBox -> Particle -> Bool
{-# INLINE withinBox #-}
withinBox (Box llx  lly rux  ruy) p
  = (px > llx) && (px <= rux) && (py > lly) && (py <= ruy)
  where
    Particle _ (Pos px py) _ = p

-- | Calculate the acceleration on a particle due to a BH Tree
--
--   If the distance between the points is less then some small number
--   we set the accel to zero to avoid the acceleration going to infinity
--   and the points escaping the simulation.
--
--   We also use this behavior as a hacky way to discard the acceleration
--   of a point due to interaction with itself.
calcAccel :: Particle -> BHTree -> Accel
calcAccel p b
  -- Particle is at the same position as the tree
  -- This check must be FIRST
  | r < small = Acc 0 0
  -- The tree has no subtrees
  -- This check must be BEFORE the closeness check
  | [] <- subtrees = Acc (aabs * dx / r) (aabs * dy / r)
  -- Particle is not close to the tree
  | r/s > 0.5 = Acc (aabs * dx / r) (aabs * dy / r)
  -- Particle is close to the tree
  | otherwise = Acc (sum xs) (sum ys)
     where
      (xs, ys) = unzip $ do
        st <- subtrees
        let Acc ax ay = calcAccel p st
        return (ax,ay)
      Pos x1 y1 = pos p
      BHT (Box minx miny maxx maxy) x2 y2 m2 subtrees = b
      dx        = x1 - x2
      dy        = y1 - y2
      rsqr      = (dx * dx) + (dy * dy)
      r         = sqrt rsqr
      s         = max (maxx - minx) (maxy - miny)
      aabs      = bigG * m2 / rsqr



