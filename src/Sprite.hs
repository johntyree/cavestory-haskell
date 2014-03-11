{-# LANGUAGE TemplateHaskell #-}
module Sprite ( Sprite
              , draw
              , makeSprite
              ) where

import Config.Config ( GraphicsQuality )
import Control.Lens ( makeLenses
                    , (^.)
                    )
import qualified Graphics.UI.SDL as SDL
import qualified SDL.Graphics as G
import Units.Length ( Length(..)
                    , asPixel
                    , Position
                    , Dimension
                    )

data Sprite = Sprite { _texture :: SDL.Texture
                     , _source  :: SDL.Rect
                     }
makeLenses ''Sprite

makeSprite :: GraphicsQuality -> Position -> Dimension -> SDL.Texture -> Sprite
makeSprite gq (x, y) (w, h) tex =
    let pixel v = fromIntegral $ asPixel gq v
    in Sprite tex (SDL.Rect (pixel x) (pixel y) (pixel w) (pixel h))

draw :: Sprite -> (Length, Length) -> G.GraphicsState ()
draw sprite pos = G.draw (sprite^.texture) pos (sprite^.source)
