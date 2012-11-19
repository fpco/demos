module Constants
  ( epsilon
  , width
  , height
  , bigG
  ) where

-- CONSTANTS --

-- For floating point comparisons
--
epsilon :: Double
epsilon = 0.001

-- Canvas attributes
--
width, height :: Int    -- extent of the window; origin is in the center
width  = 1800
height = 900

-- Gravitational constant
--
bigG :: Double
bigG = 6.67428e-11                  -- in m^3 kg^(-1) s^(-2)

