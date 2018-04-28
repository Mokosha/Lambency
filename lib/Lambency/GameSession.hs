module Lambency.GameSession (
  TimeStep, GameSession,
  physicsDeltaTime, physicsDeltaUTC, mkGameSession
) where

--------------------------------------------------------------------------------
import qualified Control.Wire as W

import GHC.Float

import Data.Time

import Lambency.Types
--------------------------------------------------------------------------------

-- The physics framerate in frames per second
physicsDeltaTime :: Double
physicsDeltaTime = 1.0 / 60.0

physicsDeltaUTC :: NominalDiffTime
physicsDeltaUTC = fromRational . toRational $ physicsDeltaTime

mkGameSession :: GameSession
mkGameSession = W.countSession (double2Float physicsDeltaTime) W.<*> W.pure ()
