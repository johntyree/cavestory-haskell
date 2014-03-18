{-# LANGUAGE TemplateHaskell #-}
module CollisionRectangle ( CollisionRectangle(..)
                          , CompositeCollisionRectangle(..)
                          , compositeCollisionRectangle
                          , Side(..)
                          ) where

import Control.Lens ( makeLenses
                    , (^.)
                    )
import qualified Rectangle as R
import Units ( Position
             , Unit(..)
             , Length(..)
             )

data Side = LeftSide | RightSide | TopSide | BottomSide
    deriving Eq

data CollisionRectangle = CollisionRectangle
    { bounds :: R.Rectangle
    , collision :: Side -> Position -> Length -> R.Rectangle
    }

data CompositeCollisionRectangle = CompositeCollisionRectangle
    { _left :: R.Rectangle
    , _right :: R.Rectangle
    , _top :: R.Rectangle
    , _bottom :: R.Rectangle
    }
makeLenses ''CompositeCollisionRectangle

compositeCollisionRectangle :: CompositeCollisionRectangle -> CollisionRectangle
compositeCollisionRectangle cr = CollisionRectangle
    { bounds = R.Rectangle (R.left $ cr^.left, R.top $ cr^.top)
                           (R.width (cr^.left) |+| R.width (cr^.right),
                            R.height (cr^.top) |+| R.height (cr^.bottom))

    , collision = collisionFunc
    }
  where
    collisionFunc LeftSide (x, y) d =
        let rect = cr^.left
        in  R.Rectangle
                (x |+| R.left rect |+| d,
                 y |+| R.top rect)
                (R.width rect |-| d,
                 R.height rect)
    collisionFunc RightSide (x, y) d =
        let rect = cr^.right
        in  R.Rectangle
                (x |+| R.left rect,
                 y |+| R.top rect)
                (R.width rect |+| d,
                 R.height rect)
    collisionFunc TopSide (x, y) d =
        let rect = cr^.top
        in  R.Rectangle
                (x |+| R.left rect,
                 y |+| R.top rect |+| d)
                (R.width rect,
                 R.height rect |-| d)
    collisionFunc BottomSide (x, y) d =
        let rect = cr^.bottom
        in  R.Rectangle
                (x |+| R.left rect,
                 y |+| R.top rect)
                (R.width rect,
                 R.height rect |+| d)
