{-# LANGUAGE TemplateHaskell #-}
module CollisionRectangle ( CollisionRectangle(..)
                          , CompositeCollisionRectangle(..)
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

class CollisionRectangle a where
    bounds :: a -> R.Rectangle
    collision :: a -> Side -> Position -> Length -> R.Rectangle

data CompositeCollisionRectangle =
    CompositeCollisionRectangle { _left :: R.Rectangle
                                , _right :: R.Rectangle
                                , _top :: R.Rectangle
                                , _bottom :: R.Rectangle
                                }
makeLenses ''CompositeCollisionRectangle

instance CollisionRectangle CompositeCollisionRectangle where
    bounds cr = R.Rectangle (R.left $ cr^.left, R.top $ cr^.top)
                            (R.width (cr^.left) |+| R.width (cr^.right),
                             R.height (cr^.top) |+| R.height (cr^.bottom))

    collision cr LeftSide (x, y) d =
        let rect = cr^.left
        in  R.Rectangle
                (x |+| R.left rect |+| d,
                 y |+| R.top rect)
                (R.width rect |-| d,
                 R.height rect)
    collision cr RightSide (x, y) d =
        let rect = cr^.right
        in  R.Rectangle
                (x |+| R.left rect,
                 y |+| R.top rect)
                (R.width rect |+| d,
                 R.height rect)
    collision cr TopSide (x, y) d =
        let rect = cr^.top
        in  R.Rectangle
                (x |+| R.left rect,
                 y |+| R.top rect |+| d)
                (R.width rect,
                 R.height rect |-| d)
    collision cr BottomSide (x, y) d =
        let rect = cr^.bottom
        in  R.Rectangle
                (x |+| R.left rect,
                 y |+| R.top rect)
                (R.width rect,
                 R.height rect |+| d)
