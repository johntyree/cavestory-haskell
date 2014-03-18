{-# LANGUAGE TemplateHaskell #-}
module PolarStar
    ( PolarStar(..)
    , initialize
    , draw
    ) where

import Config.Config ( GraphicsQuality )
import Control.Applicative
    ( (<*>)
    )
import Control.Lens
    ( makeLenses
    , use
    , (^.)
    , at
    )
import qualified Data.Map as M
import Data.Maybe
    ( fromMaybe
    )
import SDL.Graphics
    ( GraphicsState
    , loadImage
    , quality
    )
import qualified Graphics.UI.SDL as SDL
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
    , Position
    )
data Orientation = Horiz | Vert
    deriving Eq
data SpriteState = SpriteState HorizontalFacing VerticalFacing
    deriving (Eq, Ord)
type SpriteMap = M.Map SpriteState S.Sprite
type ProjectileSpriteMap = M.Map (Orientation, GunLevel) S.Sprite

data Projectile = Projectile

data PolarStar = PolarStar
    { _currentXP :: !GunExperience
    , _sprites :: !SpriteMap
    , _projectileSprites :: !ProjectileSpriteMap
    , _projectileA :: Maybe Projectile
    , _projectileB :: Maybe Projectile
    }
makeLenses ''PolarStar

allSpriteStates :: [ SpriteState ]
allSpriteStates = do
    hFacing <- [minBound..maxBound]
    vFacing <- [minBound..maxBound]
    return $ SpriteState hFacing vFacing

spriteMap :: SDL.Texture -> GraphicsQuality -> SpriteMap
spriteMap texture graphicsQuality =
    M.fromList $ zip <*> map loadSprite $ allSpriteStates
  where
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
projectileSpriteMap tex gq = M.empty

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

draw :: PolarStar -> HorizontalFacing -> VerticalFacing -> Bool -> Position -> GraphicsState ()
draw ps hFacing vFacing gunUp pos = do
    let pos' = gunPosition hFacing vFacing gunUp pos
        sprite = spriteLookup ps (SpriteState hFacing vFacing)
    S.draw sprite pos'

initialize :: GraphicsState PolarStar
initialize = do
    gunTexture <- loadImage "Arms"
    projectileTexture <- loadImage "Bullet"
    graphicsQuality <- use quality
    return $ PolarStar 0
        (spriteMap gunTexture graphicsQuality)
        (projectileSpriteMap projectileTexture graphicsQuality)
        Nothing Nothing
