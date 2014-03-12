module SDL.SDL ( withInit
               , withWindow
               , withRenderer
               ) where

import Control.Exception ( bracket )
import Data.Bits ( (.|.) )
import Foreign.C.String ( withCString )
import Foreign.C.Types ( CInt )
import qualified Graphics.UI.SDL as SDL

withInit :: (CInt -> IO a) -> IO a
withInit =
    bracket (SDL.init flags)
            (const SDL.quit)
  where
    flags = SDL.initFlagTimer .|. SDL.initFlagVideo

withWindow :: String -> (Int, Int) -> (SDL.Window -> IO a) -> IO a
withWindow title (width, height) =
    bracket (withCString title $ \cstring ->
        SDL.createWindow cstring
                         SDL.windowPosUndefined SDL.windowPosUndefined
                         (fromIntegral width) (fromIntegral height)
                         flags)
            SDL.destroyWindow
  where
    flags = 0

withRenderer :: SDL.Window -> (SDL.Renderer -> IO a) -> IO a
withRenderer window =
    bracket (SDL.createRenderer window (-1) (SDL.rendererFlagAccelerated .|.
                                             SDL.rendererFlagTargetTexture))
            SDL.destroyRenderer
