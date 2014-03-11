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
data Length = Tile TileType   |
              Pixel PixelType |
              Game GameType
type Position = (Length, Length)
type Dimension = (Length, Length)

tileSize :: Length
tileSize = Game 32.0

gameScale :: GraphicsQuality -> Double
gameScale gq = if gq == HighQuality then 1 else 2

asPixel :: GraphicsQuality -> Length -> PixelType
asPixel gq (Tile t) = fromIntegral $ t * fromIntegral (asPixel gq tileSize)
asPixel _ (Pixel p) = p
asPixel gq (Game g) = round $ g / gameScale gq

asGame :: GraphicsQuality -> Length -> GameType
asGame gq (Pixel p) = fromIntegral p * gameScale gq
asGame gq (Tile t) = fromIntegral t * asGame gq tileSize
asGame _ (Game g) = g

asTile :: GraphicsQuality -> Length -> TileType
asTile _ (Tile t) = t
asTile gq (Game g) = floor $ g / asGame gq tileSize
asTile gq (Pixel p) = fromIntegral $ p `div` asPixel gq tileSize
