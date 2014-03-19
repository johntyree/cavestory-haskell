{-# LANGUAGE TemplateHaskell #-}
module PolarStar
    ( PolarStar(..)
    , initialize
    , draw
    , startFire
    , updateProjectiles
    ) where

import Config.Config ( GraphicsQuality )
import Control.Applicative
    ( (<*>)
    )
import Control.Lens
    ( makeLenses
    , use
    , over
    , both
    , (^.)
    , (.~)
    , (%~)
    , at
    )
import qualified Data.Map as M
import Data.Maybe
    ( fromMaybe
    , isNothing
    )
import SDL.Graphics
    ( GraphicsState
    , loadImage
    , quality
    )
import qualified Graphics.UI.SDL as SDL
import qualified TileMap as TM
import qualified Sprite as S
import SpriteState
    ( HorizontalFacing(..)
    , VerticalFacing(..)
    )
import Units
    ( GunExperience
    , GunLevel
    , Length(..)
    , Unit(..)
    , CompoundUnit(..)
    , fromGamePerMS
    , Position
    , Time(..)
    )
data Orientation = Horiz | Vert
    deriving (Eq, Ord, Enum, Bounded)
data SpriteState = SpriteState HorizontalFacing VerticalFacing
    deriving (Eq, Ord)
type SpriteMap = M.Map SpriteState S.Sprite
type ProjectileSpriteMap = M.Map (Orientation, GunLevel) S.Sprite

data Projectile = Projectile
    { _sprite :: S.Sprite
    , _horizDir :: HorizontalFacing
    , _vertDir :: VerticalFacing
    , _position :: Position
    , _gunLevel :: GunLevel
    , _offset :: Length
    , _alive :: Bool
    }
makeLenses ''Projectile

data PolarStar = PolarStar
    { _currentXP :: !GunExperience
    , _sprites :: !SpriteMap
    , _projectileSprites :: !ProjectileSpriteMap
    , _projectileA :: Maybe Projectile
    , _projectileB :: Maybe Projectile
    }
makeLenses ''PolarStar

spriteMap :: SDL.Texture -> GraphicsQuality -> SpriteMap
spriteMap texture graphicsQuality =
    M.fromList $ zip <*> map loadSprite $ allSpriteStates
  where
    allSpriteStates :: [ SpriteState ]
    allSpriteStates = do
        hFacing <- [minBound..maxBound]
        vFacing <- [minBound..maxBound]
        return $ SpriteState hFacing vFacing
    loadSprite :: SpriteState -> S.Sprite
    loadSprite (SpriteState hFacing vFacing) =
        let dims = (Game 48, Tile 1)
            yHOffset HorizLeft  = Tile 0
            yHOffset HorizRight = Tile 1
            yVOffset VerticalNone = Tile 0
            yVOffset VerticalUp = Tile 2
            yVOffset VerticalDown = Tile 4
            y = yHOffset hFacing |+| yVOffset vFacing
        in  S.makeSprite graphicsQuality (Game (48 * 2), y) dims texture

projectileSpriteMap :: SDL.Texture -> GraphicsQuality -> ProjectileSpriteMap
projectileSpriteMap tex gq =
    M.fromList $ zip <*> map loadSprite $ allSpriteStates
  where
    allSpriteStates :: [ (Orientation, GunLevel) ]
    allSpriteStates = do
        orient <- [minBound..maxBound]
        level <- [1..3]
        return $ (orient, level)
    loadSprite :: (Orientation, GunLevel) -> S.Sprite
    loadSprite (orient, lvl) =
        let dims = (Tile 1, Tile 1)
            x0 2 = Tile 10
            x0 _ = Tile 8
            xOff Vert = Tile 1
            xOff _    = Tile 0
            x = x0 lvl |+| xOff orient
            y 3 = Tile 3
            y _ = Tile 2
            pos = (x, y lvl)
        in  S.makeSprite gq pos dims tex

projectileSprite :: VerticalFacing -> GunLevel -> PolarStar -> S.Sprite
projectileSprite vFacing level ps =
    fromMaybe (error "Bad projectile sprite. Check init")
              (M.lookup (orient vFacing, level) (ps^.projectileSprites))
  where
    orient VerticalNone = Horiz
    orient _            = Vert

gunPosition :: HorizontalFacing -> VerticalFacing -> Bool -> Position -> Position
gunPosition hFacing vFacing gunUp (px, py) =
    let x HorizLeft = px |-| Game 16
        x _         = px
        yOffset VerticalUp = neg $ Game 8
        yOffset VerticalDown = Game 8
        yOffset _ = Game 0
        upOffset True = neg $ Game 2
        upOffset _ = Game 0
        y = py |+| yOffset vFacing |+| upOffset gunUp
    in  (x hFacing, y)

spriteLookup :: PolarStar -> SpriteState -> S.Sprite
spriteLookup p spriteState = fromMaybe
    (error "Bad spriteLookup. Check initialization")
    $ p^.sprites.(at spriteState)

projectilePosition :: Projectile -> Position
projectilePosition p =
    let (x0, y0) = p^.position
        x VerticalNone HorizLeft  = x0 |-| p^.offset
        x VerticalNone HorizRight = x0 |+| p^.offset
        x _ _ = x0
        y VerticalUp   = y0 |-| p^.offset
        y VerticalDown = y0 |+| p^.offset
        y _ = y0
    in (x (p^.vertDir) (p^.horizDir), y (p^.vertDir))

updateProjectiles :: Time -> TM.TileMap -> PolarStar -> PolarStar
updateProjectiles t _ =
    let update :: Projectile -> Projectile
        update =
            (\p -> alive  .~ (p^.offset < Game 112) $ p) .
            (offset %~ (|+| (fromGamePerMS 0.6) |*| t))
        removeDead :: Maybe Projectile -> Maybe Projectile
        removeDead mp = do
            p <- mp
            case p^.alive of
                True -> return p
                _ -> Nothing

    in  (projectileB %~ removeDead . (fmap update)) .
        (projectileA %~ removeDead . (fmap update))

draw :: PolarStar -> HorizontalFacing -> VerticalFacing -> Bool -> Position -> GraphicsState ()
draw ps hFacing vFacing gunUp pos = do
    let pos' = gunPosition hFacing vFacing gunUp pos
        gunSprt = spriteLookup ps (SpriteState hFacing vFacing)
        drawProjectile p = S.draw (p^.sprite) (projectilePosition p)
    S.draw gunSprt pos'
    case ps^.projectileA of
        Just p -> drawProjectile p
        _ -> return ()
    case ps^.projectileB of
        Just p -> drawProjectile p
        _ -> return ()

startFire :: Position -> HorizontalFacing -> VerticalFacing -> Bool -> PolarStar -> PolarStar
startFire playerPos hFacing vFacing gunUp ps =
    let (bx0, by0) = over both (|-| Game 16) $ gunPosition hFacing vFacing gunUp playerPos
        yOff VerticalNone = Game 23
        yOff VerticalUp   = Game 4
        yOff VerticalDown = Game 28

        xOff VerticalNone HorizLeft  = Game 23
        xOff VerticalNone HorizRight = Game 23
        xOff VerticalUp   HorizLeft  = Game 27
        xOff VerticalUp   HorizRight = Game 21
        xOff VerticalDown HorizLeft  = Game 29
        xOff VerticalDown HorizRight = Game 19

        pos = (bx0 |+| xOff vFacing hFacing, by0 |+| yOff vFacing)
        mProjectileA, mProjectileB, projectile :: Maybe Projectile
        mProjectileA = ps^.projectileA
        mProjectileB = ps^.projectileB
        level = 1
        projectile = Just $ Projectile
            { _sprite = projectileSprite vFacing level ps
            , _horizDir = hFacing
            , _vertDir = vFacing
            , _position = pos
            , _gunLevel = level
            , _offset = Game 0
            , _alive = True
            }
    in  if isNothing mProjectileA
        then projectileA.~projectile $ ps
        else if isNothing mProjectileB
             then projectileB.~projectile $ ps
             else ps

initialize :: GraphicsState PolarStar
initialize = do
    gunTexture <- loadImage "Arms"
    projectileTexture <- loadImage "Bullet"
    graphicsQuality <- use quality
    return $ PolarStar 0
        (spriteMap gunTexture graphicsQuality)
        (projectileSpriteMap projectileTexture graphicsQuality)
        Nothing Nothing
