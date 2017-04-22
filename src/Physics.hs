module Physics (gravity, Field, zeroField, (./), (./.), (.+), (.+.)) where

import           Types

-- For floating point comparisons
epsilon :: Float
epsilon = 0.001

-- Gravitational constant
bigG :: Float
bigG = 6.67428e-11        -- in m^3 kg^(-1) s^(-2)

zeroVel :: Velocity
zeroVel = Vel 0 0 0

newtype Field = Field (Particle -> Force)

zeroField :: Field
zeroField = Field $ \_ -> Force 0 0 0

gravity :: Particle -> Field
gravity (Particle (Mass m0) (Pos x0 y0 z0) _) = Field f
  where
    f (Particle (Mass m1) (Pos x1 y1 z1) _)
      | d < epsilon = Force 0 0 0
      | otherwise = Force (absAccel * dx / d) (absAccel * dy / d) (absAccel * dz / d)
      where
        dx       = x0 - x1
        dy       = y0 - y1
        dz       = z0 - z1
        dsqr     = dx * dx + dy * dy + dz * dz
        d        = sqrt dsqr
        absAccel = bigG * m0 * m1 / dsqr

(./) :: Field -> Particle -> Force
Field f ./ p = f p

(./.) :: Field -> Particle -> Accel
Field f ./. p = let
  Mass m = pmass p
  Force x y z = f p in
    Accel (x / m) (y / m) (z / m)

(.+) :: Force -> Force -> Force
Force x1 y1 z1 .+ Force x2 y2 z2 = Force (x1 + x2) (y1 + y2) (z1 + z2)

(.+.) :: Field -> Field -> Field
Field f1 .+. Field f2 = Field (\x -> f1 x .+ f2 x)
