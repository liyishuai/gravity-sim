{-# LANGUAGE DeriveGeneric #-}

module Types (
  Mass(..), Position(..), Velocity(..), Accel(..), Particle(..), World(..), Force(..), Sample(..)
) where

import           Data.Aeson            hiding (Array)
import           Data.Array.Repa
import           GHC.Generics

newtype Mass   = Mass Float deriving (Show, Read, Generic) -- in kilograms
data Position  = Pos { posx :: Float, posy :: Float, posz :: Float } deriving (Show, Read, Generic) -- in meters
data Velocity  = Vel { velx :: Float, vely :: Float, velz :: Float } deriving (Show, Read, Generic) -- in meters/second
data Accel     = Accel { accx :: Float, accy :: Float, accz :: Float } deriving (Show, Read, Generic) -- in meters/second^2
data Force     = Force { fx :: Float, fy :: Float, fz :: Float} deriving (Show, Read, Generic) -- in newton

data Particle = Particle {
    pmass :: Mass
  , ppos  :: Position
  , pvel  :: Velocity
} deriving (Show, Read, Generic)

data Sample = Sample {
  spos :: Position,
  sfor :: Force
} deriving (Show, Read, Generic)

-- The world state consists of three scaling factors and a set of particles.
--
-- * The first scaling factor determines which fraction of a pixel represents one meter.
-- * The second scaling factor determines which fraction of a pixel represents
--   one kilogram when determining the radius of the circle representing a particle.
-- * The third scaling factor determines how many simulated seconds correspond to
--   one second of real time.
--
data World = World {
    seqNum        :: Int   -- sequence number to serialize communications
  , pixInM        :: Float -- fraction of a pixel corresponding to world meter
  , pixInKg       :: Float -- fraction of a pixel corresponding to world kg
  , usrToWrldTime :: Float -- user time in s to world time
  , parts         :: [Particle]
  , samples       :: [Sample]
} deriving (Show, Read, Generic)

instance FromJSON Position
instance ToJSON   Position

instance FromJSON Velocity
instance ToJSON   Velocity

instance FromJSON Mass
instance ToJSON   Mass

instance FromJSON Particle
instance ToJSON   Particle

instance FromJSON Sample
instance ToJSON   Sample

instance FromJSON Force
instance ToJSON   Force

instance FromJSON World
instance ToJSON   World
