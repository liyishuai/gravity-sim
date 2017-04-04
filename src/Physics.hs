module Physics (gravity, (./)) where

import           Types

-- For floating point comparisons
epsilon :: Float
epsilon = 0.001

-- Gravitational constant
bigG :: Float
bigG = 6.67428e-11        -- in m^3 kg^(-1) s^(-2)

newtype Field = Field (Particle -> Force)

gravity :: Particle -> Field
gravity (Particle (Mass m0) (Pos x0 y0 z0) _) = Field f
  where
    f (Particle (Mass m1) (Pos x1 y1 z1) _)
      | d < epsilon = Force 0 0 0
      | otherwise = Force (absAccel * dx / d) (absAccel * dy / d) (absAccel * dz / d)
      where
        dx       = x1 - x0
        dy       = y1 - y0
        dz       = z1 - z0
        dsqr     = dx * dx + dy * dy + dz * dz
        d        = sqrt dsqr
        absAccel = bigG * m0 * m1 / dsqr

(./) :: Force -> Mass -> Accel
(Force x y z) ./ (Mass m) = Acc (x / m) (y / m) (z / m)

(.+) :: Force -> Force -> Force
Force x1 y1 z1 .+ Force x2 y2 z2 = Force (x1 + x2) (y1 + y2) (z1 + z2)

(.+.) :: Field -> Field -> Field
Field f1 .+. Field f2 = Field (\x -> f1 x .+ f2 x)

-- Given two particles, determine the acceleration exerted by the second on the first.
--
-- As a special case, the force is zero if both particles are closer than
-- a minimal epsilon distance.
--
force :: Particle -> Particle -> Accel
force (Particle (Mass _) (Pos x1 y1 z1) _) (Particle (Mass m2) (Pos x2 y2 z2) _)
  | d < epsilon = Acc 0 0 0
  | otherwise   = Acc (absAccel * dx / d) (absAccel * dy / d) (absAccel * dz / d)
  where
    dx       = x2 - x1
    dy       = y2 - y1
    dz       = z2 - z1
    dsqr     = dx * dx + dy * dy + dz * dz
    d        = sqrt dsqr
    absAccel = bigG * m2 / dsqr
