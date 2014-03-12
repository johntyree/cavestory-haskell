{-# LANGUAGE TemplateHaskell #-}

module Player ( Player(..)
              , update
              , draw
              , initialize
              , startMovingLeft
              , startMovingRight
              , stopMoving
              , startJump
              , stopJump
              ) where

import qualified Accelerators as A
import Control.Lens ( makeLenses
                    , (^.)
                    , (.~)
                    , (%=)
                    , (.=)
                    , use
                    , _1
                    , _2
                    )
import Control.Monad ( when )
import Control.Monad.State ( State
                           )
import SDL.Graphics ( GraphicsState
                    , loadImage
                    , quality
                    )
import qualified Sprite as S
import Units ( Position
             , Velocity
             , zeroVelocity
             , sign
             , Unit(..)
             , CompoundUnit(..)
             , Time
             , Acceleration
             , Length(..)
             , fromGamePerMS
             , fromGamePerMSMS
             )

type PlayerState = State Player
data AccelDir = AccelLeft | AccelRight | AccelNone
    deriving (Show, Eq)

data Player = Player { _position :: !Position
                     , _velocity :: !(Velocity, Velocity)
                     , _sprite :: !S.Sprite
                     , _accelDir:: !AccelDir
                     , _jumpActive :: !Bool
                     , _interacting :: !Bool
                     , _onGround :: !Bool
                     }
makeLenses ''Player

jumpSpeed :: Velocity
jumpSpeed = fromGamePerMS 0.25

jumpGravity :: A.Accelerator
jumpGravity = A.constant (fromGamePerMSMS 0.0003125) (fromGamePerMS 0.2998046875)

walkAccelerationX :: Acceleration
walkAccelerationX = fromGamePerMSMS 0.00083007812

maxSpeedX :: Velocity
maxSpeedX = fromGamePerMS 0.15859375

walkLeftAccel :: A.Accelerator
walkLeftAccel = A.constant (neg walkAccelerationX) (neg maxSpeedX)

walkRightAccel :: A.Accelerator
walkRightAccel = A.constant walkAccelerationX maxSpeedX

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
    return $ Player pos (zeroVelocity, zeroVelocity) sprt AccelNone False False False

update :: Time -> PlayerState ()
update t = do
    accDir <- use accelDir
    jumpAct <- use jumpActive
    vy <- use $ velocity._2
    let yAccel
            | jumpAct && sign vy == (-1) = jumpGravity
            | otherwise = A.gravity

    velocity._2 %= (flip yAccel t)
    vy' <- use $ velocity._2
    position._2 %= (vy' |*| t |+|)
    py <- use $ position._2
    when (py >= Game 400) $ do
        onGround.=True
        position._2 .= Game 400
    when (py < Game 400) $
        onGround.=False

    ground <- use onGround
    let xAccel
            | accDir == AccelLeft = walkLeftAccel
            | accDir == AccelRight = walkRightAccel
            | otherwise = if ground
                          then walkFrictionAccel
                          else A.zero
    velocity._1 %= (flip xAccel t)
    vx <- use $ velocity._1
    position._1 %= (vx |*| t |+|)

draw :: Player -> GraphicsState ()
draw p = S.draw (p^.sprite) (p^.position)

startMovingLeft :: Player -> Player
startMovingLeft = accelDir.~AccelLeft

startMovingRight :: Player -> Player
startMovingRight = accelDir.~AccelRight

stopMoving :: Player -> Player
stopMoving = (accelDir.~AccelNone)

stopJump :: Player -> Player
stopJump = jumpActive.~False

startJump :: Player -> Player
startJump p = (jumpActive.~True) .
              (interacting.~False) .
              (velocity._2.~(if p^.onGround
                             then neg jumpSpeed
                             else p^.velocity._2)) $ p
