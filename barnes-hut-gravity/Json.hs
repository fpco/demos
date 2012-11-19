{-# LANGUAGE TemplateHaskell #-}
module Json where

import Types
import Data.Aeson
import Data.Aeson.TH

deriveJSON id ''Position
deriveJSON id ''Velocity
deriveJSON id ''Mass
deriveJSON id ''Particle
deriveJSON id ''BoundingBox
deriveJSON id ''BHTree
deriveJSON id ''World
