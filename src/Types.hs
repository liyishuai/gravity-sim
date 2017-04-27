{-# LANGUAGE DeriveGeneric #-}

module Types (
  Mass(..), Charge(..), Position(..), Velocity(..), Accel(..), Particle(..), World(..), Force(..), Sample(..), partsWorld
) where

import           Data.Aeson      hiding (Array)
import           Data.Array.Repa
import           GHC.Generics
import           Test.QuickCheck

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
  , maxCharge     :: Double -- abs value of max particle charge in Coulomb
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

instance Eq Position where
  Pos x1 y1 z1 == Pos x2 y2 z2 = let
    dx = x1 - x2
    dy = y1 - y2
    dz = z1 - z2
    sqrd = dx^2 + dy^2 + dz^2
    d = sqrt sqrd in
      d < epsilon where
    epsilon = 1e-10

instance Eq Force where
  Force x1 y1 z1 == Force x2 y2 z2 = let
    dx = x1 - x2
    dy = y1 - y2
    dz = z1 - z2
    sqrd = dx^2 + dy^2 + dz^2
    d = sqrt sqrd in
      d < epsilon where
    epsilon = 1e-10

instance Eq Sample where
  Sample p1 f1 == Sample p2 f2 = p1 == p2 && f1 == f2

instance Eq World where
  w1 == w2 = let
    samps1 = samples w1
    samps2 = samples w2 in
      samps1 == samps2

instance Arbitrary Position where
  arbitrary = do
    (x,y,z) <- arbitrary
    return $ Pos x y z

instance Arbitrary Particle where
  arbitrary = do
    m <- suchThat arbitrary (>0)
    (c,pos,vx,vy,vz) <- arbitrary
    return $ Particle (Mass m) (Charge c) pos (Vel vx vy vz)

instance Arbitrary Sample where
  arbitrary = do
    (pos,fx,fy,fz) <- arbitrary
    return $ Sample pos (Force fx fy fz)

instance Arbitrary World where
  arbitrary = do
    (sn,pm,pkg,pn,maxc) <- arbitrary
    (uwt,parts,samps) <- arbitrary
    return $ World sn pm pkg pn maxc uwt parts samps

-- | Generates an arbitrary world on given particles
partsWorld :: [Particle] -> Gen World
partsWorld ps = do
  world <- arbitrary
  return $ world { parts = ps }
