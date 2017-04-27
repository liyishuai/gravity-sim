{-# LANGUAGE DeriveGeneric #-}

module Types (
  Mass(..), Position(..), Velocity(..), Accel(..), Particle(..), World(..), Force(..), Sample(..)
) where

import           Data.Aeson      hiding (Array)
import           Data.Array.Repa
import           GHC.Generics

newtype Mass   = Mass Double deriving (Show, Read, Generic) -- in kilograms
newtype Charge = Charge Double deriving (Show, Read, Generic) -- in Coulomb
data Position  = Pos { posx :: Double, posy :: Double, posz :: Double } deriving (Show, Read, Generic) -- in meters
data Velocity  = Vel { velx :: Double, vely :: Double, velz :: Double } deriving (Show, Read, Generic) -- in meters/second
data Accel     = Accel { accx :: Double, accy :: Double, accz :: Double } deriving (Show, Read, Generic) -- in meters/second^2
data Force     = Force { fx :: Double, fy :: Double, fz :: Double} deriving (Show, Read, Generic) -- in newton

data Particle = Particle {
    pmass :: Mass
  , pchar :: Charge
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
    seqNum        :: Int    -- sequence number to serialize communications
  , pixInM        :: Double -- fraction of a pixel corresponding to world meter
  , pixInKg       :: Double -- fraction of a pixel corresponding to world kg
  , pixInN        :: Double -- fraction of a pixel corresponding to world newton
  , usrToWrldTime :: Double -- user time in s to world time
  , parts         :: [Particle]
  , samples       :: [Sample]
} deriving (Show, Read, Generic)

instance FromJSON Position
instance ToJSON   Position

instance FromJSON Velocity
instance ToJSON   Velocity

instance FromJSON Mass
instance ToJSON   Mass

instance FromJSON Charge
instance ToJSON   Charge

instance FromJSON Particle
instance ToJSON   Particle

instance FromJSON Sample
instance ToJSON   Sample

instance FromJSON Force
instance ToJSON   Force

instance FromJSON World
instance ToJSON   World
