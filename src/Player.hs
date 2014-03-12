{-# LANGUAGE TemplateHaskell #-}

module Player ( Player(..)
              , update
              , draw
              , initialize
              , startMovingLeft
              , startMovingRight
              , stopMoving
              ) where

import qualified Accelerators as A
import Control.Lens ( makeLenses
                    , (^.)
                    , (.~)
                    , (%=)
                    , (+=)
                    , use
                    , _1
                    )
import Control.Monad.State ( State
                           )
import SDL.Graphics ( GraphicsState
                    , loadImage
                    , quality
                    )
import qualified Sprite as S
import Units.Acceleration ( GamePerMSMS
                          , fromGamePerMSMS
                          )
import Units.Length ( Position
                    , Length(..)
                    )
import Units.Time ( Time )
import Units.Velocity ( Velocity
                      , zero
                      , fromGamePerMS
                      , GamePerMS
                      , delta
                      )


type PlayerState = State Player
data AccelDir = AccelLeft | AccelRight | AccelNone
    deriving Eq

data Player = Player { _position :: !Position
                     , _velocity :: !(Velocity, Velocity)
                     , _sprite :: !S.Sprite
                     , _accelDir:: !AccelDir
                     , _jumpActive :: !Bool
                     , _interacting :: !Bool
                     }
makeLenses ''Player

walkAccelerationX :: GamePerMSMS
walkAccelerationX = 0.00083007812

maxSpeedX :: GamePerMS
maxSpeedX = 0.15859375

walkLeftAccel :: A.Accelerator
walkLeftAccel = A.constant (fromGamePerMSMS (-walkAccelerationX)) (fromGamePerMS (-maxSpeedX))

walkRightAccel :: A.Accelerator
walkRightAccel = A.constant (fromGamePerMSMS walkAccelerationX) (fromGamePerMS maxSpeedX)

walkFrictionAccel :: A.Accelerator
walkFrictionAccel = A.friction (fromGamePerMSMS walkFriction)
  where
    walkFriction = 0.00049804687

initialize :: Position -> GraphicsState Player
initialize pos = do
    tex <- loadImage "MyChar"
    gq <- use quality
    let src_pos = (Tile 0, Tile 0)
        src_dim = (Tile 1, Tile 1)
        sprt = S.makeSprite gq src_pos src_dim tex
    return $ Player pos (zero, zero) sprt AccelNone False False

update :: Time -> PlayerState ()
update t = do
    accDir <- use accelDir
    let xAccel
            | accDir == AccelLeft = walkLeftAccel
            | accDir == AccelRight = walkRightAccel
            | otherwise = walkFrictionAccel

    velocity._1 %= (flip xAccel t)
    vx <- use $ velocity._1
    let dx = delta vx t
    position._1 += dx

draw :: Player -> GraphicsState ()
draw p = S.draw (p^.sprite) (p^.position)

startMovingLeft :: Player -> Player
startMovingLeft = accelDir.~AccelLeft

startMovingRight :: Player -> Player
startMovingRight = accelDir.~AccelRight

stopMoving :: Player -> Player
stopMoving = (velocity._1.~zero) . (accelDir.~AccelNone)

startJump :: Player -> Player
startJump = (jumpActive.~True) . (interacting.~False)
