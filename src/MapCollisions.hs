module MapCollisions ( update
                     , MapCollidable(..)
                     , Axis(..)
                     ) where

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
getCollisionPos tileMap r =
    let ct = filter (\(TM.CollisionTile _ tt) -> tt == TM.WallTile) $
                    TM.getCollidingTiles r tileMap
    in fmap (\(TM.CollisionTile pos _) -> pos) $
            listToMaybe ct

data Axis = AxisX | AxisY
    deriving Eq

flush :: (C.CollisionRectangle cr) =>
    C.Side -> Position -> cr -> Position -> Position
flush C.LeftSide (flushX, _) collisionRect (_, origY) =
    (flushX |+| Tile 1 |-| R.left (C.bounds collisionRect),
     origY)
flush C.RightSide (flushX, _) collisionRect (_, origY) =
    (flushX |-| R.right (C.bounds collisionRect),
     origY)
flush C.TopSide (_, flushY) collisionRect (origX, _) =
    (origX,
     flushY |+| Tile 1 |-| R.top (C.bounds collisionRect))
flush C.BottomSide (_, flushY) collisionRect (origX, _) =
    (origX,
     flushY |-| R.bottom (C.bounds collisionRect))

deltaSide :: Axis -> Length -> C.Side
deltaSide axis delta
    | sign delta == 1 = if axis == AxisX
                        then C.RightSide
                        else C.BottomSide
    | otherwise = if axis == AxisX
                  then C.LeftSide
                  else C.TopSide

oppSide :: C.Side -> C.Side
oppSide C.RightSide  = C.LeftSide
oppSide C.LeftSide   = C.RightSide
oppSide C.TopSide    = C.BottomSide
oppSide C.BottomSide = C.TopSide

applyDelta :: Axis -> Length -> Position -> Position
applyDelta AxisX dx (x, y) = (x |+| dx, y)
applyDelta _     dy (x, y) = (x, y |+| dy)

update :: (MapCollidable mc, C.CollisionRectangle cr) =>
    Axis ->
    cr ->
    Position ->
    TM.TileMap ->
    Accelerator ->
    Velocity ->
    Time ->
    State mc Position
update axis collisionRect pos tileMap acc vx0 t = do
    let vx = acc vx0 t
        delta = vx |*| t
        dSide = deltaSide axis delta

    modify $ setVelocity vx axis
    newPos <- do
        let rect = C.collision collisionRect dSide pos delta
        case getCollisionPos tileMap rect of
            Just ps -> do
                modify $ onCollision dSide True
                return $ flush dSide ps collisionRect pos
            Nothing -> do
                modify $ onDelta dSide
                return $ applyDelta axis delta pos

    let rect = C.collision collisionRect oSide newPos (Game 0)
        oSide = oppSide dSide
    case getCollisionPos tileMap rect of
        Just ps -> do
            modify $ onCollision oSide False
            return $ flush oSide ps collisionRect pos
        _ -> return newPos
