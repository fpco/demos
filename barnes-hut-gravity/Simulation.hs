module Simulation (advanceWorld) where

import Types
import Solver

-- Progressing the world state
--
advanceWorld :: Double -> World -> World
advanceWorld dtReal world = world
  { parts = newParticles
  , bhTree = Just bht
  }
  where
    -- Calculate the accelerations on each body.
    (accels, bht) = calcAccels $ parts world

    -- Apply the accelerations to the particles and advance them.
    newParticles = zipWith
      (\body (Acc ax ay) -> advanceParticle dt (Acc (-ax) (-ay)) body)
      (parts world)
      accels

    dt = dtReal * usrToWrldTime world
    -- newParticles = map (moveParticle dt) (accelerate dt $ parts world)

-- | Advance a body forwards in time.
-- Changes both the position and the velocity
advanceParticle :: Double -> Accel -> Particle -> Particle
advanceParticle time (Acc ax1 ay1) (Particle m (Pos px py) (Vel vx1 vy1)) = Particle m (Pos (px+time*vx1) (py+time*vy1)) (Vel (vx1+time*ax1) (vy1+time*ay1))


