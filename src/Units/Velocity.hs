module Units.Velocity ( Velocity(..)
                      , asGamePerMS
                      , Kinematics
                      ) where

import Config.Config ( GraphicsQuality )
import Units.Length
import Units.Time

data Velocity = Velocity Length Time |
                ZeroVelocity
type Kinematics = (Length, Velocity)

asGamePerMS :: GraphicsQuality -> Velocity -> Double
asGamePerMS gq (Velocity l t) = (asGame gq l) / (fromIntegral $ asMS t)
asGamePerMS _ (ZeroVelocity) = 0.0
