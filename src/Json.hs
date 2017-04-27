{-# LANGUAGE TemplateHaskell #-}
module Json (module Json) where

import           Data.Aeson.TH
import           JsonOpt
import           World

deriveJSON options0610 ''Position
deriveJSON options0610 ''Velocity
deriveJSON options0610 ''Mass
deriveJSON options0610 ''Particle
deriveJSON options0610 ''World
deriveJSON options0610 ''Sample
deriveJSON options0610 ''Force
