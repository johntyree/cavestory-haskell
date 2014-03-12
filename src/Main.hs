module Main (main) where

import Config.Config ( GraphicsQuality(..)
                     )
import Control.Lens ( over
                    , (%=)
                    , (.=)
                    , both
                    , use
                    , (^.)
                    )
import Control.Monad ( when
                     )
import Control.Monad.State ( execState )
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
                 , wasKeyReleased
                 , isKeyHeld
                 , keyUpEvent
                 , keyDownEvent
                 , beginNewFrame
                 )
import SDL.SDL ( withInit
               , withWindow
               , withRenderer
               )
import Units ( Length(..)
             , Time(..)
             , Unit(..)
             , asPixel
             , asMS
             , targetFrameTime
             )

initialize :: Graphics -> IO GS.GameState
initialize graphics = do
    (player, graphics') <- runStateT (Player.initialize (Tile 10, Tile 7)) graphics
    time <- fmap MS SDL.getTicks
    return $ GS.GameState player makeInput graphics' time

eventLoop :: GS.GameStateT ()
eventLoop = do
    startFrame <- fmap MS $ liftIO SDL.getTicks
    GS.input %= beginNewFrame
    input <- use GS.input
    input' <- liftIO $ with emptyEvent $ pollEvents $ input
    GS.input .= input'
    handleInput
    lastUpdate <- use GS.lastUpdate
    beforeUpdate <- fmap MS $ liftIO SDL.getTicks
    update $ beforeUpdate |-| lastUpdate
    afterUpdate <- fmap MS $ liftIO SDL.getTicks
    GS.lastUpdate .= afterUpdate
    draw
    endFrame <- fmap MS $ liftIO SDL.getTicks
    liftIO $ delay $ endFrame |-| startFrame
    when (continue input') eventLoop
  where
    delay :: Time -> IO ()
    delay t
        | t < targetFrameTime =
            SDL.delay (asMS $ targetFrameTime |-| t)
        | otherwise = return ()

    continue :: Input -> Bool
    continue i = not $ wasKeyPressed i SDL.scancodeEscape
    emptyEvent = SDL.QuitEvent 0 0

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

    handleInput :: GS.GameStateT ()
    handleInput =
        let leftKey = SDL.scancodeLeft
            rightKey = SDL.scancodeRight
            upKey = SDL.scancodeUp
            downKey = SDL.scancodeDown
            jumpKey = SDL.scancodeZ
        in do
            i <- use GS.input
            case () of
             () | (isKeyHeld i leftKey) && (isKeyHeld i rightKey) ->
                    GS.player %= Player.stopMoving
                | isKeyHeld i leftKey -> GS.player %= Player.startMovingLeft
                | isKeyHeld i rightKey ->
                    GS.player %= Player.startMovingRight
                | otherwise -> GS.player %= Player.stopMoving
            case () of
             () | (isKeyHeld i upKey) && (isKeyHeld i downKey) ->
                    GS.player %= Player.lookHorizontal
                | isKeyHeld i upKey -> GS.player %= Player.lookUp
                | isKeyHeld i downKey ->
                    GS.player %= Player.lookDown
                | otherwise -> GS.player %= Player.lookHorizontal
            case () of
             () | wasKeyPressed i jumpKey -> GS.player %= Player.startJump
                | wasKeyReleased i jumpKey -> GS.player %= Player.stopJump
                | otherwise -> return ()

    update :: Time -> GS.GameStateT ()
    update t = GS.player %= (execState (Player.update t))

    draw :: GS.GameStateT ()
    draw =
        let drawCommands :: GS.GameState -> [ GraphicsState () ]
            drawCommands gs = [ clear
                              , Player.draw (gs^.GS.player)
                              , flipBuffer
                              ]
        in  do
            gs <- get
            liftIO $ mapM_ (flip runStateT $ gs^.GS.graphics) $ drawCommands gs
            return ()

main :: IO ()
main =
    let dims = (Tile 20, Tile 15)
        gq = HighQuality
        initFunc _ =
            withWindow "Cave Story: Haskell"
                       (over both (fromIntegral . (asPixel HighQuality)) dims)
                       windowFunc
        windowFunc window = withRenderer window renderFunc
        renderFunc :: SDL.Renderer -> IO ()
        renderFunc r = initialize (Graphics r gq makeSpriteCache) >>=
            runStateT eventLoop >> return ()
    in  withInit initFunc
