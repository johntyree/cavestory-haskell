module Units.Length ( Length(..)
                    , tileSize
                    , asPixel
                    , asGame
                    , asTile
                    , Position
                    , Dimension
                    ) where

import Data.Word ( Word32 )
import Config.Config ( GraphicsQuality(..) )

type TileType = Word32
type PixelType = Int
type GameType = Double
data Length = Tile !TileType   |
              Game !GameType
  deriving Show
instance Num Length where
    (+) = opL (+)
    (*) = undefined -- Would result in Length^2
    abs = unaryL abs
    signum = unaryL signum
    fromInteger i = Game $ fromInteger i

unaryL :: (GameType -> GameType) -> Length -> Length
unaryL un l = Game (un $ asGame l)

opL :: (GameType -> GameType -> GameType) -> Length -> Length -> Length
opL op l1 l2 = Game $ op (asGame l1) (asGame l2)

type Position = (Length, Length)
type Dimension = (Length, Length)

tileSize :: GameType
tileSize = 32.0

gameScale :: GraphicsQuality -> Double
gameScale gq = if gq == HighQuality then 1 else 2

asPixel :: GraphicsQuality -> Length -> PixelType
asPixel gq v = case v of
    t@(Tile _) -> gameToPixel (asGame t)
    (Game g) -> gameToPixel g
  where
    gameToPixel game = round $ game / gameScale gq

asGame :: Length -> GameType
asGame (Tile t) = fromIntegral t * tileSize
asGame (Game g) = g

asTile :: Length -> TileType
asTile (Tile t) = t
asTile (Game g) = floor $ g / tileSize
