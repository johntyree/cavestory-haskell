module Units.Velocity ( Velocity
                      , makeVelocity
                      , fromGamePerMS
                      , asGamePerMS
                      , Kinematics
                      , delta
                      , zero
                      , GamePerMS
                      ) where

import Units.Length
import Units.Time

type GamePerMS = Double
data Velocity = Velocity !Length !Time
    deriving (Show)
instance Eq Velocity where
    v1 == v2 = (asGamePerMS v1) == (asGamePerMS v2)
instance Ord Velocity where
    v1 `compare` v2 = (asGamePerMS v1) `compare` (asGamePerMS v2)

instance Num Velocity where
    (+) = opV (+)
    (*) = undefined -- Should not be defined for velocity
    abs = unaryV abs
    signum = unaryV signum
    fromInteger i = Velocity (fromInteger i) (MS 1)

unaryV :: (Length -> Length) -> Velocity -> Velocity
unaryV un (Velocity l t) = Velocity (un l) t

opV :: (GamePerMS -> GamePerMS -> GamePerMS) -> Velocity -> Velocity -> Velocity
opV op v1 v2 = Velocity (Game $ (asGamePerMS v1) `op` (asGamePerMS v2)) (MS 1)

type Kinematics = (Length, Velocity)

makeVelocity :: Length -> Time -> Velocity
makeVelocity l t
    | ms == 0 = error "bad velocity: cannot have zero as time"
    | otherwise = Velocity l t
  where
    ms = asMS t

zero :: Velocity
zero = Velocity (Game 0) $ MS 1

fromGamePerMS :: GamePerMS -> Velocity
fromGamePerMS v = Velocity (Game v) (MS 1)

asGamePerMS :: Velocity -> GamePerMS
asGamePerMS (Velocity l t) = (asGame l) / (fromIntegral $ asMS t)

delta :: Velocity -> Time -> Length
delta v t = Game $ (asGamePerMS v) * (fromIntegral $ asMS t)
