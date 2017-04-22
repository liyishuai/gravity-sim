module Simulation (advanceWorld) where

import           Control.Parallel.Strategies
import           Data.Array.Repa             as R
import           Data.Array.Repa.Index       (DIM1)
import           Data.Array.Repa.Repr.Vector
import           Data.Vector                 as V
import           Physics
import           Types

-- Move a particle according to its velocity for the given
-- number of (simulated) seconds.
--
moveParticle :: Double -> Particle -> Particle
moveParticle dt (Particle m (Pos x y z) (Vel vx vy vz)) =
  Particle m (Pos (x + dt * vx) (y + dt * vy) (z + dt * vz)) (Vel vx vy vz)

accelerate :: Double -> Field -> Particle -> Particle
accelerate dt f p@(Particle mass pos vp) = let
  a = f ./. p
  vx = velx vp + accx a * dt
  vy = vely vp + accy a * dt
  vz = velz vp + accz a * dt in
    Particle mass pos (Vel vx vy vz)

advanceWorld :: Double -> World -> World
advanceWorld dtReal world = let
  dt = dtReal * usrToWrldTime world
  particles = fromList $ parts world
  gravityFields = V.map gravity $ particles
  gravityField = V.foldl (.+.) zeroField gravityFields
  field = gravityField
  dParticles = fromVector (ix1 (V.length particles)) particles
  newParticles = R.map (moveParticle dt . accelerate dt field) dParticles in
    world { parts = R.toList newParticles }
