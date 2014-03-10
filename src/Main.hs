module Main (main) where

import qualified Graphics.UI.SDL as SDL
import Data.Bits ( (.|.)
                 )
import Foreign.C.String ( withCString
                        )

main :: IO ()
main = do
    _ <- SDL.init $ SDL.initFlagTimer .|. SDL.initFlagVideo
    window <- withCString "Cave Story: Haskell" $ \cstring ->
        SDL.createWindow cstring SDL.windowPosUndefined SDL.windowPosUndefined 640 480 0
    renderer <- SDL.createRenderer window (-1) $
        SDL.rendererFlagAccelerated .|. SDL.rendererFlagTargetTexture

    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit
