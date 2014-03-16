module MapCollisions ( update
                     , MapCollidable(..)
                     , Axis(..)
                     ) where

import Debug.Trace
import Control.Monad ( when )

import Accelerators ( Accelerator )
import qualified CollisionRectangle as C
import Control.Monad.State ( State
                           , modify
                           )
import Data.Maybe ( listToMaybe )
import qualified Rectangle as R
import qualified TileMap as TM
import Units ( Position
             , Velocity
             , Time(..)
             , Length(..)
             , Unit(..)
             , CompoundUnit(..)
             )

class MapCollidable a where
    onCollision :: C.Side -> Bool -> a -> a
    onDelta :: C.Side -> a -> a
    setVelocity :: Velocity -> Axis -> a -> a

getCollisionPos :: TM.TileMap -> R.Rectangle -> Maybe Position
getCollisionPos tm r =
    let ct = filter (\(TM.CollisionTile pos tt) -> tt == TM.WallTile) $ TM.getCollidingTiles r tm
    in fmap (\(TM.CollisionTile pos _) -> pos) $ listToMaybe ct

data Axis = AxisX | AxisY
    deriving Eq

flush :: (C.CollisionRectangle c) => C.Side -> Position -> c -> Position -> Position
flush C.LeftSide (xf, _) crect (_, yf) = (xf |+| (Tile 1) |-| (R.left $ C.bounds crect), yf)
flush C.RightSide (xf, _) crect (_, yf) = (xf |-| (R.right $ C.bounds crect), yf)
flush C.TopSide (_, yf) crect (xf, _) = (xf, yf |+| (Tile 1) |-| (R.top $ C.bounds crect))
flush C.BottomSide (_, yf) crect (xf, _) = (xf, yf |-| (R.bottom $ C.bounds crect))

deltaSide :: Axis -> Length -> C.Side
deltaSide axis dx
    | sign dx == 1 = if axis == AxisX
                     then C.RightSide
                     else C.BottomSide
    | otherwise = if axis == AxisX
                  then C.LeftSide
                  else C.TopSide

oppSide :: C.Side -> C.Side
oppSide C.RightSide = C.LeftSide
oppSide C.LeftSide = C.RightSide
oppSide C.TopSide = C.BottomSide
oppSide C.BottomSide = C.TopSide

applyDelta :: Axis -> Length -> Position -> Position
applyDelta AxisX dx (x, y) = (x |+| dx, y)
applyDelta _ dy (x, y) = (x, y |+| dy)

update :: (MapCollidable m, C.CollisionRectangle c) =>
    Axis -> c -> Position -> TM.TileMap -> Accelerator -> Velocity -> Time -> State m Position
update axis crect pos@(x0, y0) tm acc vxb t = do
    let vx = acc vxb t
        dx = vx |*| t
        dSide = deltaSide axis dx
        oSide = oppSide dSide
        rect = C.collision crect dSide pos dx

    modify $ setVelocity vx axis
    newPos <- case getCollisionPos tm rect of
        Just ps -> do
            modify $ onCollision dSide True
            return $ flush dSide ps crect pos
        Nothing -> do
            modify $ onDelta dSide
            return $ applyDelta axis dx pos

    let rect' = C.collision crect oSide newPos (Game 0)
    case getCollisionPos tm rect' of
        Just ps -> do
            modify $ onCollision oSide False
            return $ flush oSide ps crect pos
        _ -> return newPos
