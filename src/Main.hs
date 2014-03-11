module Main (main) where

import Config.Config ( GraphicsQuality(..) )
import Control.Lens ( over
                    , both
                    , (^.)
                    )
import Control.Monad ( when
                     )
import Control.Monad.Trans ( liftIO )
import Control.Monad.Trans.State ( runStateT
                                 , get
                                 )
import Foreign.Marshal.Utils ( with )
import Foreign.Ptr ( Ptr )
import Foreign.Storable ( peek )
import qualified GameState as GS
import qualified Graphics.UI.SDL as SDL
import qualified Player as Player
import SDL.Graphics ( Graphics(..)
                    , GraphicsState
                    , makeSpriteCache
                    , clear
                    , flipBuffer
                    )
import SDL.Input ( Input
                 , makeInput
                 , wasKeyPressed
                 , keyUpEvent
                 , keyDownEvent
                 )
import SDL.SDL ( withInit
               , withWindow
               , withRenderer
               )
import Units.Length ( Length(..)
                    , asPixel
                    )

initialize :: Graphics -> IO GS.GameState
initialize graphics = do
    (player, graphics') <- runStateT (Player.initialize (Tile 10, Tile 7)) graphics
    return $ GS.GameState player makeInput graphics'

pollEvents :: Input -> Ptr SDL.Event -> IO Input
pollEvents input event_ptr =
    SDL.pollEvent event_ptr >>= \result ->
        if result == 0
        then return input
        else peek event_ptr >>= \event ->
                let input' = processEvent input event
                in pollEvents input' event_ptr
  where
    processEvent :: Input -> SDL.Event -> Input
    processEvent i e
        | e_type == SDL.eventTypeKeyDown = keyDownEvent i e
        | e_type == SDL.eventTypeKeyUp = keyUpEvent i e
        | otherwise = i
      where e_type = SDL.eventType e

drawCommands :: GS.GameState -> [ GraphicsState () ]
drawCommands gs = [ clear
                  , Player.draw (gs^.GS.player)
                  , flipBuffer
                  ]

eventLoop :: GS.GameStateT ()
eventLoop = do
    gs <- get
    input <- liftIO $ with emptyEvent $ pollEvents $ gs^.GS.input
    liftIO $ mapM_ (flip runStateT $ gs^.GS.graphics) $ drawCommands gs
    when (continue input) eventLoop
  where
    continue :: Input -> Bool
    continue i = not $ wasKeyPressed i SDL.scancodeEscape
    emptyEvent = SDL.QuitEvent 0 0

main :: IO ()
main = do
    let dims = (Tile 20, Tile 15)
        gq = HighQuality
        initFunc _ =
            withWindow "Cave Story: Haskell"
                       (over both (asPixel HighQuality) dims)
                       windowFunc
        windowFunc window = withRenderer window renderFunc
        renderFunc :: SDL.Renderer -> IO ()
        renderFunc r = initialize (Graphics r gq makeSpriteCache) >>=
            runStateT eventLoop >> return ()
    withInit initFunc
