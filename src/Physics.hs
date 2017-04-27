module Physics (bigG, gravity, electroMagnetic, Field, zeroCharge, zeroVel, zeroField, posPart, (./), (./.), (+.), (.+.)) where

import           Types

-- For floating point comparisons
epsilon :: Double
epsilon = 0.001

-- Gravitational constant
bigG :: Double
bigG = 6.67428e-11        -- in m^3 kg^(-1) s^(-2)

-- Coulomb's constant
ke :: Double
ke = 8.9875517873681764e9 -- in m^3 kg^(-1) s^(-2) C^(-2)

oneKG :: Mass
oneKG = Mass 1

zeroCharge :: Charge
zeroCharge = Charge 0

sampleCharge :: Charge
sampleCharge = Charge 1.5e-10

zeroVel :: Velocity
zeroVel = Vel 0 0 0

posPart :: Position -> Particle
posPart pos = Particle oneKG sampleCharge pos zeroVel

newtype Field = Field (Particle -> Force)

zeroField :: Field
zeroField = Field $ \_ -> Force 0 0 0

gravity :: Particle -> Field
gravity (Particle (Mass m0) _ (Pos x0 y0 z0) _) = Field f
  where
    f (Particle (Mass m1) _ (Pos x1 y1 z1) _)
      | d < epsilon = Force 0 0 0
      | otherwise = Force (absAccel * dx / d) (absAccel * dy / d) (absAccel * dz / d)
      where
        dx       = x0 - x1
        dy       = y0 - y1
        dz       = z0 - z1
        dsqr     = dx^2 + dy^2 + dz^2
        d        = sqrt dsqr
        absAccel = bigG * m0 * m1 / dsqr

electroMagnetic :: Particle -> Field
electroMagnetic (Particle _ (Charge c0) (Pos x0 y0 z0) _) = Field f
  where
    f (Particle _ (Charge c1) (Pos x1 y1 z1) _)
      | d < epsilon = Force 0 0 0
      | otherwise = Force (absAccel * dx / d) (absAccel * dy / d) (absAccel * dz / d)
      where
        dx = x1 - x0
        dy = y1 - y0
        dz = z1 - z0
        dsqr = dx^2 + dy^2 + dz^2
        d = sqrt dsqr
        absAccel = ke * c0 * c1 / dsqr

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

(+.) :: (Particle -> Field) -> (Particle -> Field) -> Particle -> Field
(pf1 +. pf2) p = pf1 p .+. pf2 p
