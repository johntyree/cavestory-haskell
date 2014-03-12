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
              , lookUp
              , lookHorizontal
              , lookDown
              ) where
import qualified Player.WalkingAnimation as WA

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
data VerticalFacing = VerticalNone | VerticalUp | VerticalDown
    deriving (Eq, Enum, Bounded, Ord)
data MotionType = Standing | Interacting | Walking | Jumping | Falling
    deriving (Eq, Enum, Bounded, Ord)

data SpriteState = SpriteState HorizontalFacing VerticalFacing MotionType WA.Stride
    deriving (Eq, Ord)
type SpriteMap = Map.Map SpriteState S.Sprite

data Player = Player { _position :: !Position
                     , _velocity :: !(Velocity, Velocity)
                     , _sprites :: !SpriteMap
                     , _accelDir:: !AccelDir

                     , _horizFacing :: !HorizontalFacing
                     , _intendedVertFacing :: !VerticalFacing

                     , _walkingAnimation :: !WA.WalkingAnimation

                     , _jumpActive :: !Bool
                     , _interacting :: !Bool
                     , _onGround :: !Bool
                     }
makeLenses ''Player

jumpSpeed :: Velocity
jumpSpeed = fromGamePerMS 0.25

jumpGravity :: A.Accelerator
jumpGravity = A.constant (fromGamePerMSMS 0.0003125) (fromGamePerMS 0.2998046875)

airAccelerationX :: Acceleration
airAccelerationX = fromGamePerMSMS 0.0003125

walkAccelerationX :: Acceleration
walkAccelerationX = fromGamePerMSMS 0.00083007812

maxSpeedX :: Velocity
maxSpeedX = fromGamePerMS 0.15859375

airLeftAccel :: A.Accelerator
airLeftAccel = A.constant (neg airAccelerationX) (neg maxSpeedX)

airRightAccel :: A.Accelerator
airRightAccel = A.constant airAccelerationX maxSpeedX

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
    vFacing <- [minBound .. maxBound]
    motion <- [minBound .. maxBound]
    stride <- [minBound .. maxBound]
    return $ SpriteState hFacing vFacing motion stride

spriteMap :: SDL.Texture -> GraphicsQuality -> SpriteMap
spriteMap tex gq =
    Map.fromList $ map loadSprite allSpriteStates
  where
    loadSprite :: SpriteState -> (SpriteState, S.Sprite)
    loadSprite state@(SpriteState hFacing vFacing motion stride) =
        let dims = (Tile 1, Tile 1)
            y = if hFacing == HorizLeft
                then Tile 0
                else Tile 1
            x = if vFacing == VerticalDown
                then Tile 6
                else case motion of
                    Walking -> case stride of
                        WA.StrideMiddle -> Tile 0
                        WA.StrideLeft -> Tile 1
                        WA.StrideRight -> Tile 2
                    Standing -> Tile 0
                    Interacting -> Tile 7
                    Jumping -> Tile 1
                    Falling -> Tile 2
            xOff = if vFacing == VerticalUp
                   then Tile 3
                   else Tile 0
            x' = xOff |+| x
        in (state, S.makeSprite gq (x', y) dims tex)

initialize :: Position -> GraphicsState Player
initialize pos = do
    tex <- loadImage "MyChar"
    gq <- use quality
    let sprtMp = spriteMap tex gq
    return $ Player
        pos
        (zeroVelocity, zeroVelocity)
        sprtMp
        AccelNone
        (HorizLeft)
        (VerticalNone)
        WA.makeWalkingAnimation
        False
        False
        False

update :: Time -> PlayerState ()
update t = do
    walkingAnimation %= (WA.update t)

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
            | accDir == AccelLeft = if ground
                                    then walkLeftAccel
                                    else airLeftAccel
            | accDir == AccelRight = if ground
                                     then walkRightAccel
                                     else airRightAccel
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
            vFacing = if p^.onGround && p^.intendedVertFacing == VerticalDown
                      then VerticalNone
                      else p^.intendedVertFacing
            motion
                | p^.interacting = Interacting
                | p^.onGround = if p^.accelDir == AccelNone
                                then Standing
                                else Walking
                | otherwise = if sign (p^.velocity._2) == 1
                              then Jumping
                              else Falling
            stride = WA.stride $ p^.walkingAnimation
        in  SpriteState hFacing vFacing motion stride

startMovingLeft :: Player -> Player
startMovingLeft = (accelDir.~AccelLeft) .
                  (horizFacing.~HorizLeft)

startMovingRight :: Player -> Player
startMovingRight = (accelDir.~AccelRight) .
                   (horizFacing.~HorizRight)

stopMoving :: Player -> Player
stopMoving = (accelDir.~AccelNone)

lookUp :: Player -> Player
lookUp = (interacting.~False) .
         (intendedVertFacing.~VerticalUp)

lookDown :: Player -> Player
lookDown p =
    if p^.intendedVertFacing /= VerticalDown
    then (interacting.~grounded) .
         (intendedVertFacing.~VerticalDown) $ p
    else p
  where
    grounded = p^.onGround

lookHorizontal :: Player -> Player
lookHorizontal = intendedVertFacing.~VerticalNone

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
