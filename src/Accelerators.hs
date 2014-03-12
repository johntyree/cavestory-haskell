module Accelerators ( zero
                    , friction
                    , constant
                    , Accelerator
                    ) where

import Units.Time ( Time )
import qualified Units.Velocity as V
import Units.Acceleration ( Acceleration
                          , deltaV
                          , posA
                          )

type Accelerator = V.Velocity -> Time -> V.Velocity

zero :: V.Velocity -> Time -> V.Velocity
zero v _ = v

friction :: Acceleration -> V.Velocity -> Time -> V.Velocity
friction f v t
    | signum v == 1 = max V.zero $ v - (deltaV f t)
    | otherwise = min V.zero (v + (deltaV f t))

constant :: Acceleration -> V.Velocity -> V.Velocity -> Time -> V.Velocity
constant a terminalV v t
    | posA a = min (v + (deltaV a t)) terminalV
    | otherwise = max (v + (deltaV a t)) terminalV
