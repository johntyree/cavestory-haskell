{-# LANGUAGE TemplateHaskell #-}
module GameState ( GameState(..)
                 , player
                 , input
                 , graphics
                 , GameStateT
                 , lastUpdate
                 ) where

import Control.Lens ( makeLenses )
import Control.Monad.Trans.State ( StateT )
import SDL.Graphics ( Graphics )
import SDL.Input ( Input )
import Player ( Player )
import Units.Time ( Time )

type GameStateT = StateT GameState IO
data GameState = GameState { _player :: !Player
                           , _input :: !Input
                           , _graphics :: !Graphics
                           , _lastUpdate :: !Time
                           }
makeLenses ''GameState
