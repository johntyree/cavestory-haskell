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
              , startFire
              , stopFire
              ) where
import qualified Player.WalkingAnimation as WA

import qualified Accelerators as A
import qualified CollisionRectangle as C
import Config.Config ( GraphicsQuality )
import Control.Applicative ( (<*>) )
import Control.Lens
    ( makeLenses
    , (^.)
    , (.~)
    , (%~)
    , (%=)
    , assign
    , use
    , _1
    , _2
    )
import Control.Lens.At ( at )
import Control.Monad.State ( State
                           )
import qualified Data.Map as Map
import qualified MapCollisions as MC
import Data.Maybe ( fromMaybe )
import qualified Graphics.UI.SDL as SDL
import qualified PolarStar as PS
import qualified Rectangle as R
import SDL.Graphics ( GraphicsState
                    , loadImage
                    , quality
                    )
import qualified Sprite as S
import SpriteState
    ( HorizontalFacing(..)
    , VerticalFacing(..)
    )
import qualified TileMap as TM
import Units ( Position
             , Velocity
             , zeroVelocity
             , sign
             , Unit(..)
             , Time
             , Acceleration
             , Length(..)
             , fromGamePerMS
             , fromGamePerMSMS
             )

type PlayerState = State Player
data AccelDir = AccelLeft | AccelRight | AccelNone
    deriving (Show, Eq)

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
                     , _polarStar :: !PS.PolarStar

                     , _jumpActive :: !Bool
                     , _interacting :: !Bool
                     , _onGround :: !Bool
                     }
makeLenses ''Player

instance MC.MapCollidable Player where
    onCollision C.BottomSide True =
        (velocity._2.~zeroVelocity) .
        (onGround.~True)
    onCollision C.BottomSide _ =
        (onGround.~True)
    onCollision C.TopSide True =
        (velocity._2.~zeroVelocity)
    onCollision _ True =
        (velocity._1.~zeroVelocity)
    onCollision _ _ = id

    onDelta C.BottomSide =
        (onGround.~False)
    onDelta C.TopSide =
        (onGround.~False)
    onDelta _ = id

    setVelocity v MC.AxisX = velocity._1.~v
    setVelocity v MC.AxisY = velocity._2.~v

collisionRectangle :: C.CollisionRectangle
collisionRectangle =
    C.compositeCollisionRectangle $
        C.CompositeCollisionRectangle
            (R.Rectangle (Game  6, Game 10) (Game 10, Game 12))
            (R.Rectangle (Game 16, Game 10) (Game 10, Game 12))
            (R.Rectangle (Game  7, Game  2) (Game 18, Game 15))
            (R.Rectangle (Game 11, Game 17) (Game 10, Game 15))

jumpSpeed :: Velocity
jumpSpeed = fromGamePerMS 0.25

jumpGravity :: A.Accelerator
jumpGravity = A.constant (fromGamePerMSMS 0.0003125) A.terminalSpeed

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

allSpriteStates :: [SpriteState]
allSpriteStates = do
    hFacing <- [minBound .. maxBound]
    vFacing <- [minBound .. maxBound]
    motion  <- [minBound .. maxBound]
    stride  <- [minBound .. maxBound]
    return $ SpriteState hFacing vFacing motion stride

spriteMap :: SDL.Texture -> GraphicsQuality -> SpriteMap
spriteMap texture graphicsQuality =
    Map.fromList $ zip <*> map loadSprite $ allSpriteStates
  where
    loadSprite :: SpriteState -> S.Sprite
    loadSprite (SpriteState hFacing vFacing motion stride) =
        let dims = (Tile 1, Tile 1)
            y HorizLeft = Tile 0
            y _         = Tile 1
            x0 VerticalDown _       _               = Tile 6
            x0 _            Walking WA.StrideMiddle = Tile 0
            x0 _            Walking WA.StrideLeft   = Tile 1
            x0 _            Walking WA.StrideRight  = Tile 2
            x0 _            Standing    _           = Tile 0
            x0 _            Interacting _           = Tile 7
            x0 _            Jumping     _           = Tile 1
            x0 _            Falling     _           = Tile 2
            xOffset VerticalUp = Tile 3
            xOffset _          = Tile 0
            x = (xOffset vFacing) |+| (x0 vFacing motion stride)
        in S.makeSprite graphicsQuality (x, y hFacing) dims texture

initialize :: Position -> GraphicsState Player
initialize pos = do
    texture <- loadImage "MyChar"
    graphicsQuality <- use quality
    ps <- PS.initialize
    let sprtMp = spriteMap texture graphicsQuality
    return $ Player
        pos
        (zeroVelocity, zeroVelocity)
        sprtMp
        AccelNone
        (HorizLeft)
        (VerticalNone)
        WA.makeWalkingAnimation
        ps
        False
        False
        False

update :: TM.TileMap -> Time -> PlayerState ()
update tm t = do
    walkingAnimation %= (WA.update t)
    polarStar %= (PS.updateProjectiles t tm)
    do  -- Update Y
        jumpAct <- use jumpActive
        vy <- use $ velocity._2
        let yAccel
                | jumpAct && sign vy == (-1) = jumpGravity
                | otherwise = A.gravity

        pos <- use position
        (MC.update MC.AxisY collisionRectangle pos tm yAccel vy t) >>=
            assign position

    do  -- Update X
        let xAccel' AccelLeft  True  = walkLeftAccel
            xAccel' AccelLeft  False = airLeftAccel
            xAccel' AccelRight True  = walkRightAccel
            xAccel' AccelRight False = airRightAccel
            xAccel' _          True  = walkFrictionAccel
            xAccel' _          False = A.zero

        accDir <- use accelDir
        grounded <- use onGround
        let xAccel = xAccel' accDir grounded
        vx <- use $ velocity._1
        pos <- use position
        (MC.update MC.AxisX collisionRectangle pos tm xAccel vx t) >>=
            assign position

gunUp :: Player -> Bool
gunUp p = (motionType p) == Walking && (WA.stride (p^.walkingAnimation)) /= WA.StrideMiddle

draw :: Player -> GraphicsState ()
draw p = do
    PS.draw (p^.polarStar) (p^.horizFacing) (verticalFacing p) (gunUp p) (p^.position)
    S.draw (spriteLookup p) (p^.position)

verticalFacing :: Player -> VerticalFacing
verticalFacing p = vFacing' (p^.onGround) (p^.intendedVertFacing)
  where
    vFacing' True VerticalDown = VerticalNone
    vFacing' _    intended     = intended

motionType :: Player -> MotionType
motionType p = motion' (p^.interacting) (p^.onGround) (p^.accelDir)
  where
    motion' True _    _         = Interacting
    motion' _    True AccelNone = Standing
    motion' _    True _         = Walking
    motion' _ _ _
        | sign (p^.velocity._2) == 1 = Jumping
        | otherwise                  = Falling
spriteLookup :: Player -> S.Sprite
spriteLookup p = fromMaybe
    (error "Bad spriteLookup. Check initialization")
    $ p^.sprites.(at state)
  where
    state =
        let hFacing = p^.horizFacing
            vFacing = verticalFacing p
            motion = motionType p
            stride = WA.stride $ p^.walkingAnimation
        in  SpriteState hFacing vFacing motion stride

startMovingLeft :: Player -> Player
startMovingLeft = (accelDir.~AccelLeft) .
                  (horizFacing.~HorizLeft) .
                  (interacting.~False)

startMovingRight :: Player -> Player
startMovingRight = (accelDir.~AccelRight) .
                   (horizFacing.~HorizRight) .
                   (interacting.~False)

stopMoving :: Player -> Player
stopMoving = accelDir.~AccelNone

lookUp :: Player -> Player
lookUp = (interacting.~False) .
         (intendedVertFacing.~VerticalUp)

lookDown :: Player -> Player
lookDown p = lookDown' $ p^.intendedVertFacing
  where
    lookDown' VerticalDown = p
    lookDown' _            =
        (interacting.~grounded) .
        (intendedVertFacing.~VerticalDown) $ p
    grounded = p^.onGround

lookHorizontal :: Player -> Player
lookHorizontal = intendedVertFacing.~VerticalNone

stopFire :: Player -> Player
stopFire = id

startFire :: Player -> Player
startFire p = polarStar %~
    (PS.startFire (p^.position)
                  (p^.horizFacing)
                  (verticalFacing p)
                  (gunUp p)) $ p

stopJump :: Player -> Player
stopJump = jumpActive.~False

startJump :: Player -> Player
startJump = (jumpActive.~True) .
            (interacting.~False) .
            setVelocity
  where
    newVelocity p
        | p^.onGround = neg jumpSpeed
        | otherwise   = p^.velocity._2
    setVelocity p = velocity._2 .~ (newVelocity p) $ p
