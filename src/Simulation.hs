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
moveParticle dt (Particle m c (Pos x y z) (Vel vx vy vz)) =
  Particle m c (Pos (x + dt * vx) (y + dt * vy) (z + dt * vz)) (Vel vx vy vz)

accelerate :: Double -> Field -> Particle -> Particle
accelerate dt f p@(Particle mass c pos vp) = let
  a = f ./. p
  vx = velx vp + accx a * dt
  vy = vely vp + accy a * dt
  vz = velz vp + accz a * dt in
    Particle mass c pos (Vel vx vy vz)

updateSample :: Field -> [Sample] -> [Sample]
updateSample f samps = let
  vSamples = fromList samps
  newSamps = V.map (\s -> s { sfor = f ./ (posPart (spos s))} ) vSamples in
    V.toList newSamps

advanceWorld :: Double -> World -> World
advanceWorld dt world = let
  particles = fromList $ parts world
  gravityFields = V.map gravity $ particles
  gravityField = V.foldl (.+.) zeroField gravityFields
  field = gravityField
  dParticles = fromVector (ix1 (V.length particles)) particles
  newParticles = R.map (moveParticle dt . accelerate dt field) dParticles in
    world { parts = R.toList newParticles,
            samples = updateSample field $ samples world }
