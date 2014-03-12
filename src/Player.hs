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
import Config.Config ( GraphicsQuality )
import Control.Lens ( makeLenses
                    , (^.)
                    , (.~)
                    , (%=)
                    , (.=)
                    , use
                    , _1
                    , _2
                    )
import Control.Lens.At ( at )
import Control.Monad ( when )
import Control.Monad.State ( State
                           )
import qualified Data.Map as Map
import Data.Maybe ( fromMaybe )
import qualified Graphics.UI.SDL as SDL
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

data HorizontalFacing = HorizLeft | HorizRight
    deriving (Eq, Enum, Bounded, Ord)

data SpriteState = SpriteState HorizontalFacing
    deriving (Eq, Ord)
type SpriteMap = Map.Map SpriteState S.Sprite

data Player = Player { _position :: !Position
                     , _velocity :: !(Velocity, Velocity)
                     , _sprites :: !SpriteMap
                     , _accelDir:: !AccelDir

                     , _horizFacing :: !HorizontalFacing

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

allSpriteStates :: [ SpriteState ]
allSpriteStates = do
    hFacing <- [minBound .. maxBound]
    return $ SpriteState hFacing

spriteMap :: SDL.Texture -> GraphicsQuality -> SpriteMap
spriteMap tex gq =
    Map.fromList $ map loadSprite allSpriteStates
  where
    loadSprite :: SpriteState -> (SpriteState, S.Sprite)
    loadSprite state@(SpriteState hFacing) =
        let dims = (Tile 1, Tile 1)
            y = if hFacing == HorizLeft
                then Tile 0
                else Tile 1
            x = Tile 0
        in (state, S.makeSprite gq (x, y) dims tex)

initialize :: Position -> GraphicsState Player
initialize pos = do
    tex <- loadImage "MyChar"
    gq <- use quality
    let sprtMp = spriteMap tex gq
    return $ Player pos (zeroVelocity, zeroVelocity) sprtMp AccelNone (HorizLeft) False False False

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
draw p = S.draw (spriteLookup p) (p^.position)

spriteLookup :: Player -> S.Sprite
spriteLookup p = fromMaybe
    (error "Bad spriteLookup. Check initialization")
    $ p^.sprites.(at state)
  where
    state =
        let hFacing = p^.horizFacing
        in  SpriteState hFacing

startMovingLeft :: Player -> Player
startMovingLeft = (accelDir.~AccelLeft) .
                  (horizFacing.~HorizLeft)

startMovingRight :: Player -> Player
startMovingRight = (accelDir.~AccelRight) .
                   (horizFacing.~HorizRight)

stopMoving :: Player -> Player
stopMoving = (accelDir.~AccelNone)

stopJump :: Player -> Player
stopJump = jumpActive.~False

startJump :: Player -> Player
startJump = (jumpActive.~True) .
            (interacting.~False) .
            setVelocity
  where
    setVelocity :: Player -> Player
    setVelocity p = velocity._2 .~
        (if p^.onGround
         then neg jumpSpeed
         else p^.velocity._2) $ p
