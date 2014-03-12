module Accelerators ( zero
                    , friction
                    , constant
                    , gravity
                    , Accelerator
                    ) where

import Units ( Time
             , Velocity
             , zeroVelocity
             , Acceleration
             , Unit(..)
             , CompoundUnit(..)
             , sign
             , fromGamePerMSMS
             , fromGamePerMS
             )

type Accelerator = Velocity -> Time -> Velocity

zero :: Accelerator
zero v _ = v

friction :: Acceleration -> Accelerator
friction f v t
    | sign v == 1 = max zeroVelocity $ v |-| f |*| t
    | otherwise = min zeroVelocity (v |+| f |*| t)

constant :: Acceleration -> Velocity -> Accelerator
constant a terminalV v t
    | sign a == 1 = min (v |+| a |*| t) terminalV
    | otherwise = max (v |+| a |*| t) terminalV

gravity :: Accelerator
gravity = constant (fromGamePerMSMS 0.00078125) (fromGamePerMS 0.2998046875)
