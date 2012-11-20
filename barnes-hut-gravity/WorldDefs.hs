{-# LANGUAGE ScopedTypeVariables #-}

module WorldDefs
  ( simpleWorld  -- A simple world
  , gridWorld  -- A grid!
  , solarWorld -- Solar system simulation
  , world4     -- a 4-body world
  , galaxyWorld  -- Nice!
  -- read a world from a file
  , readWorld
  ) where

import Constants
import Types

import Prelude hiding     (catch)

import Control.Exception  (catch)
import System.Exit        (exitFailure)


simpleWorld :: World
simpleWorld = World 0 distanceScale massScale (750*750)
                      [ Particle (Mass sunMass) (Pos 0 0) (Vel 0 0)
                      , Particle (Mass earthMass) (Pos earthDistance 0) (Vel 0 earthVelocity)
                      ]
                      Nothing
  where
    sunMass         = 1.9891e30
    earthDistance   = 152098232e3   -- Aphelion
    earthMass       = 5.9736e24
    earthVelocity   = 29.78e3
    distanceScale = (fromIntegral height * 0.4) / earthDistance
    massScale = earthMass / 1e4

gridWorld :: World
gridWorld = World 0 distanceScale (mass/10000) 750 particles Nothing
  where
    distance = 1e10
    mass = 1e33
    particles = map makePart [Pos (x*distance) (y*distance) | x <- [-5..5], y <- [-5..5]]
    makePart p = Particle (Mass mass) p (Vel 0 0)
    distanceScale = (fromIntegral height * 0.05) / distance


solarWorld :: World
solarWorld = World 0 distanceScale (earthMass / 10000) (750*750)
                      [ Particle (Mass sunMass) (Pos 0 0) (Vel 0 0)
                      , Particle (Mass cometMass) (Pos cometDistance 0) (Vel 0 cometVelocity)
                      , Particle (Mass cometMass) (Pos (-cometDistance) (-cometDistance)) (Vel (-5000) 5000)
                      , Particle (Mass earthMass) (Pos earthDistance 0) (Vel 0 earthVelocity)
                      , Particle (Mass venusMass) (Pos venusDistance 0) (Vel 0 venusVelocity)
                      , Particle (Mass mercuryMass) (Pos mercuryDistance 0) (Vel 0 mercuryVelocity)]
                      Nothing
  where
    sunMass         = 1.9891e30
    earthDistance   = 152098232e3   -- Aphelion
    earthMass       = 5.9736e24
    earthVelocity   = 29.78e3
    venusDistance   = 1.08e11
    venusMass       = 4.869e24
    venusVelocity   = 35e3
    mercuryDistance = 4.6e10
    mercuryMass     = 3.3e23
    mercuryVelocity = 49.88e3
    cometDistance   = 2.0e11
    cometMass       = 1.0e20
    cometVelocity   = 7000
    --
    distanceScale = (fromIntegral height * 0.4) / earthDistance

world4 :: World
world4 = World 0 5e10 9.42590890872e11 1000
               [ Particle (Mass 1e16) (Pos (-100e9) 30e9) (Vel 0 (-65))
               , Particle (Mass 1e16) (Pos 240e9 0)     (Vel (-40) 30)
               , Particle (Mass 1e16) (Pos 50e9 200e9)    (Vel 0 (-30))
               , Particle (Mass 1e15) (Pos 0 (-300e9))  (Vel 0 5)]
               Nothing

galaxyWorld :: World
galaxyWorld = World 0 distanceScale (mass/10000) 750 particles Nothing
  where
    distance = 1e11
    mass = 1e34
    sunMass = 1e38
    particles = [ Particle (Mass sunMass) (Pos 0 0) (Vel 0 0) ] ++ particles'
    particles' = map makePart [Pos (x*distance) (y*distance) | x <- [-3..3], y <- [-3..3], (x,y) /= (0,0)]
    makePart p = Particle (Mass mass) p (startVel 380 p)
    distanceScale = (fromIntegral height * 0.005) / distance
    -- | Get the starting velocity of a body.
    --   It is set to rotate around the origin, with the speed proportional
    --   to the sqrt of the distance from it. This seems to make nice simulations.
    startVel :: Double -> Position -> Velocity
    startVel scale p@(Pos x y) = Vel vx vy
      where
        (vx, vy)       = mulSV (sqrt (magV (x,y)) * scale) (y', -x')
        (x', y')       = normaliseV (x, y)
        normaliseV v   = mulSV (1 / magV v) v
        mulSV s (x, y) = (s * x, s * y)
        magV (x, y)    = sqrt (x * x + y * y)



-- Setting up the world
-- --------------------

-- Read a world model from the given file
--
readWorld :: FilePath -> IO World
readWorld fname
  = do
      contents <- readFile fname
      readIO contents
   `catch` \(exc::IOError) ->
     do
       putStrLn $ "Fatal error: can't read world description\n" ++ show exc
       exitFailure


