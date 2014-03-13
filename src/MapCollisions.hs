module MapCollisions ( update
                     {-, MapCollidable(..)-}
                     , Axis(..)
                     ) where


import Accelerators ( Accelerator )
import qualified CollisionRectangle as C
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

{-class MapCollidable a where-}
    {-onCollision :: C.Side -> Bool -> a -> a-}
    {-onDelta :: C.Side -> a -> a-}

getCollisionPos :: TM.TileMap -> R.Rectangle -> Maybe Position
getCollisionPos tm r =
    let ct = filter (\(TM.CollisionTile pos tt) -> tt == TM.WallTile) $ TM.getCollidingTiles r tm
    in fmap (\(TM.CollisionTile pos _) -> pos) $ listToMaybe ct

data Axis = AxisX | AxisY
    deriving Eq

update :: (C.CollisionRectangle c) =>
    Axis -> c -> Position -> TM.TileMap -> Accelerator -> Velocity -> Time -> Position
update axis crect pos@(xb, yb) tm acc vxb t =
    let vx = acc vxb t
        dx = vx |*| t
        deltaSide
            | sign dx == 1 = if axis == AxisX
                             then C.RightSide
                             else C.BottomSide
            | otherwise = if axis == AxisX
                          then C.LeftSide
                          else C.TopSide
        oppSide
            | sign dx /= 1 = if axis == AxisX
                             then C.RightSide
                             else C.BottomSide
            | otherwise = if axis == AxisX
                          then C.LeftSide
                          else C.TopSide

        rect = C.collision crect deltaSide pos dx
        flush C.LeftSide (xf, _) = xf |+| (Tile 1) |-| (R.left $ C.bounds crect)
        flush C.RightSide (xf, _) = xf |-| (R.right $ C.bounds crect)
        flush C.TopSide (_, yf) = yf |+| (Tile 1) |-| (R.top $ C.bounds crect)
        flush C.BottomSide (_, yf) = yf |-| (R.bottom $ C.bounds crect)
        x = case getCollisionPos tm rect of
                Just ps -> flush deltaSide ps
                Nothing -> xb |+| dx
        rect' = C.collision crect oppSide pos dx
        x' = case getCollisionPos tm rect' of
                Just ps -> flush oppSide ps
                _ -> x
    in (x', yb)


{-update :: (C.CollisionRectangle c, MapCollidable m) => c -> Accelerator -> Position ->-}
          {-(Velocity, Velocity) -> Time -> TM.TileMap -> (Position, Velocity)-}
