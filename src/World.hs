{-# LANGUAGE ScopedTypeVariables #-}

module World (
  -- read a world from a file
  readWorld,

  solarWorld,
  -- a 4-body world
  world4
) where

import           Control.Exception (catch)
import           Physics
import           System.Exit       (exitFailure)
import           Types

width, height :: Int    -- extent of the window; origin is in the center
width  = 600
height = 600

-- Read a world model from the given file
--
readWorld :: FilePath -> IO World
readWorld fname
  = do
      contents <- readFile fname
      readIO contents
   `catch` \(exc::IOError) ->
     do
       putStrLn $ "Fatal error: can't read world description\n" ++ show exc
       exitFailure

solarWorld :: World
solarWorld = World 0 distanceScale (earthMass / 10000) 800 1e6
                      [ Particle (Mass sunMass) zeroCharge
                                 (Pos 0 0 0) (Vel 0 0 0)
                      , Particle (Mass cometMass) zeroCharge
                                 (Pos cometDist 0 0) (Vel 0 cometVelocity 0)
                      , Particle (Mass cometMass) zeroCharge
                                 (Pos (-cometDist) (-cometDist) 0) (Vel 5000 (-5000) 0)
                      , Particle (Mass cometMass) zeroCharge
                                 (Pos 2.0e11 1.0e11 0) (Vel (-2500) 5000 0)
                      , Particle (Mass earthMass) zeroCharge
                                 (Pos earthDist  0 0) (Vel 0 earthVelocity 0)
                      , Particle (Mass venusMass) zeroCharge
                                 (Pos venusDist  0 0) (Vel 0 venusVelocity 0)
                      , Particle (Mass mercuryMass) zeroCharge
                                 (Pos mercuryDist  0 0) (Vel 0 mercuryVelocity 0)]
                      getSamples
  where
    sunMass         = 1.9891e30
    earthDist       = 152098232e3   -- Aphelion
    earthMass       = 5.9736e24
    earthVelocity   = 29543.941651224563
    venusDist       = 1.08e11
    venusMass       = 4.869e24
    venusVelocity   = 35060.543334193906
    mercuryDist     = 4.6e10
    mercuryMass     = 3.3e23
    mercuryVelocity = 53721.929673328814
    cometDist       = 2.0e11
    cometMass       = 1.0e20
    cometVelocity   = 7000
    distanceScale = (fromIntegral height * 0.4) / earthDist
    getSamples =
      concatMap (plotSamplesCircle 12) [earthDist, venusDist, mercuryDist]

plotSamplesCircle :: Int -> Double -> [Sample]
plotSamplesCircle n r =
  map (\i -> Sample (Pos (cos (ang i) * r) (sin (ang i) * r) 0) (Force 0 0 0))
      [0..n - 1]
  where ang i = 2 * pi / realToFrac n * realToFrac i

world4 :: World
world4 = World 0 0.5 9.42590890872e11 1 1
               [ Particle (Mass 1e16) zeroCharge (Pos (-100) 30 0) (Vel 0 (-65) 0)
               , Particle (Mass 1e16) zeroCharge (Pos 240 0 0)     (Vel (-40) 30 0)
               , Particle (Mass 1e16) zeroCharge (Pos 50 200 0)    (Vel 0 (-30) 0)
               , Particle (Mass 1e15) zeroCharge (Pos 0 (-300) 0)  (Vel 0 5 0)]
               getSamples
  where getSamples = plotSamplesGrid 4 4 scale
        scale      = 0.5

plotSamplesGrid :: Int -> Int -> Double -> [Sample]
plotSamplesGrid m n scale =
  map (\(x, y) -> Sample (Pos (minx + ux * x) (miny + uy * y) 0) (Force 0 0 0))
      [(realToFrac x, realToFrac y) | x <- [1..m], y <- [1..n]]
  where minx = -realToFrac width  / 2 / scale
        miny = -realToFrac height / 2 / scale
        ux   =  realToFrac width  / realToFrac (m + 1) / scale
        uy   =  realToFrac height / realToFrac (n + 1) / scale
