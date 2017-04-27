import           Physics
import           Simulation
import           Test.QuickCheck
import           Types
import           World

-- | Orbital period of planets in solar system
period :: Double -> Double
period radius = 2 * pi * sqrt (radius ^ 3 / (bigG * sunMass))
  where sunMass = 1.9891e30

earthPeriod, venusPeriod, mercuryPeriod :: Double
earthPeriod = period 152098232e3
venusPeriod = period 1.08e11
mercuryPeriod = period 4.6e10

-- | Simulate a world for a period of time
simWorld :: Double -> Double -> World -> World
simWorld t dt = let
  n = ceiling $ t / dt in
  (!!n) . iterate (advanceWorld dt)

-- | Two particles are far from each other
(!=) :: Particle -> Particle -> Property
(!=) (Particle _ _ (Pos x1 y1 z1) _) (Particle _ _ (Pos x2 y2 z2) _) = let
  distance = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2) in
    counterexample ("distance = " ++ show distance) (distance > epsilon)
  where
    epsilon = 1e8

-- | Check period of planet
prop_period :: Int -> Double -> Double -> Property
prop_period index period dt = let
  oldPlanet = parts solarWorld !! index
  newWorld = simWorld period dt solarWorld
  newPlanet = parts newWorld !! index in
  oldPlanet != newPlanet

forAlldt :: (Arbitrary a, Ord a, Num a, Show a, Testable prop) =>
  (a -> prop) -> Property
forAlldt = forAll (suchThat arbitrary (>100))

prop_earth = expectFailure . forAlldt $ prop_period 4 earthPeriod
prop_venus = expectFailure . forAlldt $ prop_period 5 venusPeriod
prop_mercury = expectFailure . forAlldt $ prop_period 6 mercuryPeriod

-- | An arbitrary static particle
staticParticle :: Gen Particle
staticParticle = do
  p <- arbitrary
  return $ p { pvel = zeroVel }

-- | Helper function to modify the charge of particle for testing
modifyCharge :: Particle -> Particle -> Particle
modifyCharge p0@(Particle (Mass m0) (Charge c0) _ _)
             p1@(Particle (Mass m1) _ _ _) =
  p1 { pchar = Charge (bigG * m0 * m1 / (ke * c0)) }

-- | Check if a two-particle system is stable
prop_2parts :: Particle -> Particle -> Property
prop_2parts p1 p2' = forAll (partsWorld [p1,p2]) prop_stable where
  p2 = modifyCharge p1 p2'
  prop_stable oldWorld t1 t2 = let
    newWorld1 = advanceWorld t1 oldWorld
    newWorld2 = advanceWorld t2 oldWorld in
      newWorld1 == newWorld2

main :: IO()
main = do
  quickCheck prop_earth
  quickCheck prop_venus
  quickCheck prop_mercury
  quickCheck prop_2parts
