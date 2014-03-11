{-# LANGUAGE TemplateHaskell #-}

module Player ( Player(..)
              , update
              , draw
              , initialize
              , startMovingLeft
              , startMovingRight
              ) where

import Control.Lens ( makeLenses
                    , (^.)
                    , (.~)
                    , (%~)
                    , use
                    )
import SDL.Graphics ( GraphicsState
                    , loadImage
                    , quality
                    )
import qualified Sprite as S
import Units.Length ( Position
                    , Length(..)
                    )
import Units.Time ( Time )
import Units.Velocity ( Velocity(..) )


data AccelDir = AccelLeft | AccelRight | AccelNone

data Player = Player { _position :: Position
                     , _velocity :: Velocity
                     , _sprite :: S.Sprite
                     , _accelDir:: AccelDir
                     }
makeLenses ''Player

initialize :: Position -> GraphicsState Player
initialize pos = do
    tex <- loadImage "MyChar"
    gq <- use quality
    let src_pos = (Tile 0, Tile 0)
        src_dim = (Tile 1, Tile 1)
        sprt = S.makeSprite gq src_pos src_dim tex
    return $ Player pos ZeroVelocity sprt AccelNone

update :: Time -> Player -> Player
update _ player = undefined

draw :: Player -> GraphicsState ()
draw p = S.draw (p^.sprite) (p^.position)

startMovingLeft :: Player -> Player
startMovingLeft = accelDir.~AccelLeft

startMovingRight :: Player -> Player
startMovingRight = accelDir.~AccelRight

stopMoving :: Player -> Player
stopMoving = accelDir.~AccelNone
