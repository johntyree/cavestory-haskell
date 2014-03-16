module TileMap ( TileType(..)
               , CollisionTile(..)
               , TileMap
               , makeTestMap
               , getCollidingTiles
               , draw
               {-, drawBackground-}
               ) where

import Control.Lens ( use
                    , over
                    , both
                    )
import qualified Data.Foldable as F
import Data.List ( foldl' )
import qualified Data.Sequence as Seq
import qualified Rectangle as R
import SDL.Graphics ( GraphicsState
                    , loadImage
                    , quality
                    )
import qualified Sprite as S
import qualified Units as U

data TileType = AirTile |
                WallTile
    deriving Eq

data CollisionTile = CollisionTile !U.Position !TileType

data Tile = Tile !TileType (Maybe S.Sprite)

type Grid a = Seq.Seq (Seq.Seq a)
type TileGrid = Seq.Seq (Seq.Seq Tile)
type BgGrid = Seq.Seq (Seq.Seq (Maybe S.Sprite))
data TileMap = TileMap !TileGrid !BgGrid

makeTestMap :: GraphicsState TileMap
makeTestMap = do
    tex <- loadImage "PrtCave"
    gq <- use quality
    let sprt = S.makeSprite gq (U.Tile 1, U.Tile 0) (U.Tile 1, U.Tile 1) tex
        wallT = Tile WallTile (Just sprt)
        tGrid = fromListWithDefault (Tile AirTile Nothing)
                                    (U.Tile 20, U.Tile 15)
                                    ([((U.Tile x, U.Tile (16 - x)), wallT) | x <- [6..8]] ++
                                     [((U.Tile x, U.Tile x), wallT) | x <- [7..10]] ++
                                     [((U.Tile x, U.Tile 11), wallT) | x <- [0..20]])
        bgGrid = Seq.empty
    return $ TileMap tGrid bgGrid

draw :: TileMap -> GraphicsState ()
draw (TileMap tg _) = do
    let sprites = foldrWithPosition (\pos (Tile _ s) list ->
            case s of
                Just sprt -> (sprt, pos):list
                Nothing -> list) [] tg
    mapM_ (\(pos, sprt) -> S.draw pos sprt) sprites

{-drawBackground :: TileMap -> GraphicsState ()-}

getCollidingTiles :: R.Rectangle -> TileMap -> [CollisionTile]
getCollidingTiles r (TileMap tg _) =
    let tiles = fmap F.toList $ section (R.left r, R.top r) (R.right r, R.bottom r)
            (zipIndex tg)
    in map (\(pos, Tile typ _) -> CollisionTile pos typ) (F.concat tiles)

zipIndex :: Grid a -> Grid (U.Position, a)
zipIndex grid = Seq.zipWith rowZip grid $ Seq.fromList [0..Seq.length grid]
  where
    rowZip row y = Seq.zipWith (colZip y) row $ Seq.fromList [0..Seq.length row]
    colZip y tile x = ((U.Tile x, U.Tile y), tile)

-- Returns a sub-section of Grid a
section :: U.Position -> U.Position -> Grid a -> Grid a
section pos1 pos2 grid =
    let ((x1,y1),(x2,y2)) = over both (over both U.asTile) (pos1, pos2)
        grid' = Seq.take (y2-y1+1) (Seq.drop y1 grid)
    in  fmap (\row -> Seq.take (x2-x1+1) (Seq.drop x1 row)) grid'

foldrWithPosition :: (U.Position -> a -> b -> b) -> b -> Grid a -> b
foldrWithPosition f acc grid =
    Seq.foldrWithIndex (\y row yAcc ->
        Seq.foldrWithIndex (\x val xAcc -> f (U.Tile x, U.Tile y) val xAcc)
            yAcc row) acc grid

fromListWithDefault :: a -> U.Dimension -> [(U.Position, a)] -> Grid a
fromListWithDefault a (w, h) list =
    let width = U.asTile w
        height = U.asTile h
        def = Seq.replicate height $ Seq.replicate width a
        inserts =
            let mapFunc (pos, val) =
                    let (x, y) = over both U.asTile pos
                    in (\grid -> Seq.update y (Seq.update x val (Seq.index grid y)) grid)
            in  map mapFunc list
    in foldl' (\grid f -> f grid) def inserts
