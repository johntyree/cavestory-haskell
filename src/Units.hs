{-# LANGUAGE FunctionalDependencies#-}
module Units ( Length(..)
             , Time(..)
             , Position
             , Dimension
             , Velocity
             , Acceleration
             , zeroVelocity
             , zeroAcceleration
             , fromGamePerMS
             , fromGamePerMSMS
             , Unit(..)
             , CompoundUnit(..)
             , asPixel
             , asMS
             , targetFrameTime
             , Frame
             , FrameRate(..)
             , asTimePerFrame
             ) where

import Config.Config ( GraphicsQuality(..) )
import Data.Word ( Word32 )

type Frame = Word32
data FrameRate = FrameRate !Frame !Time
    deriving Show

asTimePerFrame :: FrameRate -> Time
asTimePerFrame (FrameRate f t) = MS $ asMS t `div` f

type GameUnit = Double
type TileUnit = Int
data Length = Game !GameUnit |
              Tile !TileUnit
    deriving Show
type Position = (Length, Length)
type Dimension = Position

type TimeUnit = Word32
data Time = S TimeUnit |
            MS TimeUnit
    deriving Show
type GamePerMS = Double
data Velocity = Velocity !Length !Time
    deriving Show
type GamePerMSMS = Double
data Acceleration = Acceleration !Velocity !Time
    deriving Show

infixl 7 |*|
infixl 6 |+|
infixl 6 |-|

class Unit a where
    (|+|), (|-|) :: a -> a -> a
    neg :: a -> a
    sign :: a -> Int
    x |-| y = x |+| neg y

class (Unit denom, Unit numer) =>
    CompoundUnit denom numer compound |
    compound denom -> numer where
    (|*|) :: compound -> denom -> numer

instance Eq Length where
    (==) = eqImpl asGame
instance Ord Length where
    compare = compImpl asGame
instance Unit Length where
    l1 |+| l2 = Game $ asGame l1 + asGame l2
    neg l = Game $ negate $ asGame l
    sign l = round $ signum $ asGame l

instance Eq Time where
    (==) = eqImpl asMS
instance Ord Time where
    compare = compImpl asMS
instance Unit Time where
    t1 |+| t2 = MS $ asMS t1 + asMS t2
    neg t = MS $ negate $ asMS t
    sign t = fromIntegral $ signum $ asMS t

instance Eq Velocity where
    (==) = eqImpl asGamePerMS
instance Ord Velocity where
    compare = compImpl asGamePerMS
instance Unit Velocity where
    t1 |+| t2 = fromGamePerMS $ asGamePerMS t1 + asGamePerMS t2
    neg t = fromGamePerMS $ negate $ asGamePerMS t
    sign t = round $ signum $ asGamePerMS t
instance CompoundUnit Time Length Velocity where
    v |*| t = Game $ (asGamePerMS v) * (fromIntegral $ asMS t)

instance Eq Acceleration where
    (==) = eqImpl asGamePerMSMS
instance Ord Acceleration where
    compare = compImpl asGamePerMSMS
instance Unit Acceleration where
    t1 |+| t2 = fromGamePerMSMS $ asGamePerMSMS t1 + asGamePerMSMS t2
    neg t = fromGamePerMSMS $ negate $ asGamePerMSMS t
    sign t = round $ signum $ asGamePerMSMS t
instance CompoundUnit Time Velocity Acceleration where
    a |*| t = fromGamePerMS $ (asGamePerMSMS a) * (fromIntegral $ asMS t)

eqImpl :: Eq b => (a -> b) -> a -> a -> Bool
eqImpl f a b = f a == f b

compImpl :: Ord b => (a -> b) -> a -> a -> Ordering
compImpl f a b = f a `compare` f b

tileSize :: GameUnit
tileSize = 32.0

asGame :: Length -> GameUnit
asGame (Game g) = g
asGame (Tile t) = fromIntegral t * tileSize

asMS :: Time -> TimeUnit
asMS (S t) = 1000 * t
asMS (MS t) = t

zeroVelocity :: Velocity
zeroVelocity = fromGamePerMS 0.0

zeroAcceleration :: Acceleration
zeroAcceleration = fromGamePerMSMS 0.0

asGamePerMS :: Velocity -> GamePerMS
asGamePerMS (Velocity l t) = (asGame l) / (fromIntegral $ asMS t)

fromGamePerMS :: GamePerMS -> Velocity
fromGamePerMS gpms = Velocity (Game gpms) (MS 1)

asGamePerMSMS :: Acceleration -> GamePerMS
asGamePerMSMS (Acceleration v t) = (asGamePerMS v) / (fromIntegral $ asMS t)

fromGamePerMSMS :: GamePerMSMS -> Acceleration
fromGamePerMSMS gpmsms = Acceleration (fromGamePerMS gpmsms) (MS 1)

targetFps :: Int
targetFps = 60

targetFrameTime :: Time
targetFrameTime = MS $ 1000 `div` fromIntegral targetFps

type PixelType = Word32
asPixel :: GraphicsQuality -> Length -> PixelType
asPixel gq l =
    let gameScale
            | gq == HighQuality = 1.0
            | otherwise = 0.5
    in  round $ (asGame l) * gameScale
