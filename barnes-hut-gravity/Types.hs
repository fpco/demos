module Types (

  -- types
  Mass(..), Position(..), Velocity(..), Accel(..), Energy, Particle(..), BHTree(..), BoundingBox(..), World(..),

) where

-- Basic physical measures
--
newtype Mass   = Mass Double
  deriving (Show, Read) -- in kilogram
data Position  = Pos { x  :: Double, y  :: Double } deriving (Show, Read) -- in meter
data Velocity  = Vel { vx :: Double, vy :: Double } deriving (Show, Read) -- in meter/second
data Accel     = Acc { ax :: Double, ay :: Double } deriving (Show, Read) -- in meter/second^2
type Energy    = Double             -- in joule

-- We represent particles as mass points at a particular position that have a particular velocity
--
data Particle = Particle {
    mass :: Mass
  , pos  :: Position
  , vel  :: Velocity
} deriving (Show, Read)

-- The world state consists of three scaling factors and a set of particles.
--
-- * The first scaling factor determines which fraction of a pixel represents one meter.
-- * The second scaling factor determines which fraction of a pixel represents one kilogram when
--   determining the radius of the circle representing a particle.
-- * The third scaling factor determines how many simulated seconds correspond to one second of real
--   time.
--
data World = World {
    seqNum  :: Int   -- sequence number to serialize communications
  , pixInM  :: Double -- fraction of a pixel corresponding to world meter
  , pixInKg :: Double -- fraction of a pixel corresponding to world kg
  , usrToWrldTime :: Double -- user time in s to world time
  , parts   :: [Particle]
  , bhTree  :: Maybe BHTree
} deriving (Show, Read)

-- | A rectangular region in 2D space.
data BoundingBox
  = Box
  { boxLowerLeftX  :: {-# UNPACK #-} !Double
  , boxLowerLeftY  :: {-# UNPACK #-} !Double
  , boxUpperRightX :: {-# UNPACK #-} !Double
  , boxUpperRightY :: {-# UNPACK #-} !Double
  }
  deriving (Show, Read, Eq)

-- | The Barnes-Hut tree we use to organise the points.
data BHTree
  = BHT
  { bhTreeBox     :: {-# UNPACK #-} !BoundingBox
  , bhTreeCenterX :: {-# UNPACK #-} !Double
  , bhTreeCenterY :: {-# UNPACK #-} !Double
  , bhTreeMass    :: {-# UNPACK #-} !Double
  , bhTreeBranch  :: ![BHTree]
  }
  deriving (Show, Read, Eq)


