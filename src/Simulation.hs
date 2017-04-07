module Simulation (moveParticle) where

import           Control.Parallel.Strategies
import           Data.Array.Accelerate
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
  a = f ./ p
  vx = velx vp + accx a * dt
  vy = vely vp + accy a * dt
  vz = velz vp + accz a * dt in
    Particle mass pos (Vel vx vy vz)

-- Accelerate a particle in dependence on the gravitational force
-- exerted by all other particles for
-- the given number of (simulated) seconds.
-- force :: Particle -> Particle -> Accel
{-
accelerate :: Float -> [Particle] -> [Particle]
accelerate dt particles =
    parMap rseq acc particles
  where
    acc particle =
      foldl addAcc particle particles
    addAcc myParticle@(Particle m pos (Vel vx vy)) otherParticle =
      let (Acc ax ay) = force myParticle otherParticle
      in
        Particle m pos (Vel (vx + dt * ax) (vy + dt * ay))

-- Progressing the world state
--
advanceWorld :: Float -> World -> World
advanceWorld dtReal world =
  let dt = dtReal * usrToWrldTime world
      newParticles = map (moveParticle dt) (accelerate dt $ parts world)
  in
      world { parts = newParticles }
-}
