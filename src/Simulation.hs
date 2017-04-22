module Simulation (moveParticle) where

import           Control.Parallel.Strategies
import           Data.Array.Repa             as R
import           Data.Array.Repa.Repr.Vector
import           Data.Vector                 as V
import           Physics
import           Types

-- Move a particle according to its velocity for the given
-- number of (simulated) seconds.
--
moveParticle :: Float -> Particle -> Particle
moveParticle dt (Particle m (Pos x y z) (Vel vx vy vz)) =
  Particle m (Pos (x + dt * vx) (y + dt * vy) (z + dt * vz)) (Vel vx vy vz)

accelerate :: Float -> Field -> Particle -> Particle
accelerate dt f p@(Particle mass pos vp) = let
  a = f ./. p
  vx = velx vp + accx a * dt
  vy = vely vp + accy a * dt
  vz = velz vp + accz a * dt in
    Particle mass pos (Vel vx vy vz)

advanceWorld :: Float -> World -> World
advanceWorld dtReal world = let
  dt = dtReal * usrToWrldTime world
  gravityFields = V.map gravity $ fromList $ parts world
  gravityField = V.foldl (.+.) zeroField gravityFields
  field = gravityField
  dParticles = fromListVector Z $ parts world
  newParticles = R.map (moveParticle dt . accelerate dt field) dParticles in
    world { parts = R.toList newParticles }
