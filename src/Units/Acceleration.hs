module Units.Acceleration ( Acceleration
                          , makeAcceleration
                          , asGamePerMSMS
                          , deltaV
                          , posA
                          , GamePerMSMS
                          , fromGamePerMSMS
                          ) where

import Units.Length
import Units.Velocity
import Units.Time

type GamePerMSMS = Double
data Acceleration = Acceleration !Velocity !Time

posA :: Acceleration -> Bool
posA (Acceleration v _) = signum v == 1

makeAcceleration :: Velocity -> Time -> Acceleration
makeAcceleration v t
    | ms == 0 = error "bad acceleration: cannot have zero as time"
    | otherwise = Acceleration v t
  where
    ms = asMS t

fromGamePerMSMS :: GamePerMSMS -> Acceleration
fromGamePerMSMS val = Acceleration (fromGamePerMS val) (MS 1)

asGamePerMSMS :: Acceleration -> GamePerMSMS
asGamePerMSMS (Acceleration v t) = asGamePerMS v / (fromIntegral $ asMS t)

deltaV :: Acceleration -> Time -> Velocity
deltaV a t = makeVelocity (Game $ (asGamePerMSMS a) * (fromIntegral $ asMS t)) (MS 1)
