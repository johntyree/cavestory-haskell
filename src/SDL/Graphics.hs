{-# LANGUAGE TemplateHaskell #-}
module SDL.Graphics ( SpriteCache
                    , loadImage
                    , Graphics(..)
                    , GraphicsState
                    , makeSpriteCache
                    , draw
                    , clear
                    , flipBuffer
                    , renderer
                    , spriteCache
                    , quality
                    ) where

import Config.Config ( GraphicsQuality(..) )
import Control.Exception ( bracket )
import Control.Lens ( makeLenses
                    , (^.)
                    , (.~)
                    , use
                    )
import Control.Monad ( when
                     )
import Control.Monad.Trans ( liftIO )
import Control.Monad.Trans.State ( StateT
                                 , get
                                 , put
                                 )
import qualified Data.Map as Map
import Foreign.C.String ( withCString )
import Foreign.Marshal.Utils ( with )
import Foreign.Storable ( peek )
import Foreign.Ptr ( nullPtr )
import qualified Graphics.UI.SDL as SDL
import Units.Length ( Length(..)
                    , asPixel
                    )

type GraphicsState = StateT Graphics IO
type SpriteCache = Map.Map String SDL.Texture
data Graphics = Graphics { _renderer :: SDL.Renderer
                         , _quality :: GraphicsQuality
                         , _spriteCache :: SpriteCache
                         }
makeLenses ''Graphics

makeSpriteCache :: SpriteCache
makeSpriteCache = Map.empty

imagePath :: GraphicsQuality -> String -> String
imagePath (HighQuality) name = "content/" ++ name ++ ".bmp"
imagePath (OriginalQuality) name = "content/original_graphics/" ++ name ++ ".pbm"

renderFunc :: (SDL.Renderer -> IO a) -> GraphicsState ()
renderFunc func = use renderer >>= liftIO . func >> return ()

clear :: GraphicsState ()
clear = renderFunc SDL.renderClear

flipBuffer :: GraphicsState ()
flipBuffer = renderFunc SDL.renderPresent

-- TODO: Reader
draw :: SDL.Texture -> (Length, Length) -> SDL.Rect -> GraphicsState ()
draw texture (x, y) src = do
    rend <- use renderer
    gq <- use quality
    let pixel v = fromIntegral $ asPixel gq v
        dst = SDL.Rect (pixel x) (pixel y)
                       (SDL.rectW src)
                       (SDL.rectH src)
    _ <- liftIO $ with dst $ \dst_ptr ->
        with src $ \src_ptr ->
            SDL.renderCopy rend texture src_ptr dst_ptr
    return ()

-- TODO: Writer
loadImage :: String -> GraphicsState SDL.Texture
loadImage name = do
    g <- get
    (texture, graphics) <- liftIO $ loadImage' g name
    put graphics
    return texture

loadImage' :: Graphics -> String -> IO (SDL.Texture, Graphics)
loadImage' graphics name =
    let gq = graphics^.quality
        rend = graphics^.renderer
        sprites = graphics^.spriteCache
        path = imagePath gq name
        lookup_result = Map.lookup path sprites
        createAndInsertTexture surface_ptr = do
            when (surface_ptr == nullPtr) $
                error $ "Could not find image at " ++ path
            surface <- peek surface_ptr
            black <- SDL.mapRGB (SDL.surfaceFormat surface) 0 0 0
            let enable_color_key = 1
            _ <- SDL.setColorKey surface_ptr enable_color_key black
            texture <- SDL.createTextureFromSurface rend surface_ptr
            return (texture, spriteCache .~ Map.insert path texture sprites $ graphics)

    in case lookup_result of
        Just texture -> return (texture, graphics)
        Nothing ->
            bracket (withCString path SDL.loadBMP)
                    SDL.freeSurface
                    createAndInsertTexture
