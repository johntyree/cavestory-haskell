module Main (main) where

import Config.Config ( GraphicsQuality(..)
                     )
import Control.Lens ( over
                    , (%=)
                    , (.=)
                    , assign
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
import qualified TileMap as TileMap
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
    ((player, tileMap), graphics') <- flip runStateT graphics $ do
        player <- Player.initialize (Tile 10, Tile 2)
        tileMap <- TileMap.makeTestMap
        return (player, tileMap)
    time <- fmap MS SDL.getTicks
    return $ GS.GameState player tileMap makeInput graphics' time

eventLoop :: GS.GameStateT ()
eventLoop = do
    startFrame <- fmap MS $ liftIO SDL.getTicks
    -- Collect Input
    do  GS.input %= beginNewFrame
        input <- use GS.input
        input' <- liftIO $ with emptyEvent $ pollEvents $ input
        GS.input .= input'
        handleInput

    -- Update
    do  lastUpdate <- use GS.lastUpdate
        do  beforeUpdate <- fmap MS $ liftIO SDL.getTicks
            update $ beforeUpdate |-| lastUpdate
        (fmap MS $ liftIO SDL.getTicks) >>= assign GS.lastUpdate

    -- Draw
    draw

    -- V-Sync
    do  endFrame <- fmap MS $ liftIO SDL.getTicks
        liftIO $ delay (endFrame |-| startFrame)

    -- Continue loop
    do  input <- use GS.input
        when (continue input) eventLoop
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
            | e_type == SDL.eventTypeKeyDown &&
              SDL.keyboardEventRepeat e == 0 = keyDownEvent i e
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
            fireKey = SDL.scancodeX
            -- counteractInput: when two inputs (e.g. <-/-> counteract each
            -- other)
            counteractInput i a aact b bact cact
                | (isKeyHeld i a) && (isKeyHeld i b) = GS.player %= cact
                | isKeyHeld i a = GS.player %= aact
                | isKeyHeld i b = GS.player %= bact
                | otherwise = GS.player %= cact
        in do
            i <- use GS.input
            counteractInput i leftKey Player.startMovingLeft
                                  rightKey Player.startMovingRight
                                  Player.stopMoving
            counteractInput i upKey Player.lookUp
                                  downKey Player.lookDown
                                  Player.lookHorizontal
            when (wasKeyPressed i jumpKey) $ GS.player %= Player.startJump
            when (wasKeyReleased i jumpKey) $ GS.player %= Player.stopJump
            when (wasKeyPressed i fireKey) $ GS.player %= Player.startFire
            when (wasKeyReleased i fireKey) $ GS.player %= Player.stopFire

    update :: Time -> GS.GameStateT ()
    update t = do
        tm <- use GS.tileMap
        GS.player %= (execState (Player.update tm t))

    draw :: GS.GameStateT ()
    draw =
        let drawCommands :: GS.GameState -> [ GraphicsState () ]
            drawCommands gs = [ clear
                              , Player.draw (gs^.GS.player)
                              , TileMap.draw (gs^.GS.tileMap)
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
