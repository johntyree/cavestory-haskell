module MapCollisions ( updateX
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
             , CompoundUnit(..)
             )

getCollisionPos :: TM.TileMap -> R.Rectangle -> Maybe Position
getCollisionPos tm r =
    let ct = filter (\(TM.CollisionTile pos tt) -> tt == TM.WallTile) $ TM.getCollidingTiles r tm
    in fmap (\(TM.CollisionTile pos _) -> pos) $ listToMaybe ct

updateX :: C.CollisionRectangle a => a -> Accelerator -> Position ->
           (Velocity, Velocity) -> Time -> TM.TileMap -> (Position, Velocity)
updateX crect acc p@(x, y) (vxb, vy) dt tm =
    let vx = acc vxb dt
        d = vx |*| dt
    in  undefined

updateY :: C.CollisionRectangle a => a -> Accelerator -> Position ->
           (Velocity, Velocity) -> Time -> TM.TileMap -> (Position, Velocity)
updateY crect acc (x, y) (vx, vy) dt tm = undefined










