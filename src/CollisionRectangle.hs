{-# LANGUAGE TemplateHaskell #-}
module CollisionRectangle ( CollisionRectangle(..)
                          , CompositeCollisionRectangle(..)
                          , Side(..)
                          {-, Orientation(..)-}
                          ) where

import Control.Lens ( makeLenses
                    , (^.)
                    )
import qualified Rectangle as R
import Units ( Position
             , Unit(..)
             , Length(..)
             )

{-data Orientation = OrientVertical | OrientHorizontal-}
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
        R.Rectangle
            (x |+| (R.left $ cr^.left) |+| d,
             y |+| (R.top $ cr^.left))
            ((R.width $ cr^.left) |-| d,
             R.height $ cr^.left)
    collision cr RightSide (x, y) d =
        R.Rectangle
            (x |+| (R.left $ cr^.right),
             y |+| (R.top $ cr^.right))
            ((R.width $ cr^.right) |+| d,
             R.height $ cr^.right)
    collision cr TopSide (x, y) d =
        R.Rectangle
            (x |+| (R.left $ cr^.top),
             y |+| (R.top $ cr^.top) |+| d)
            (R.width $ cr^.top,
             (R.height $ cr^.top) |-| d)
    collision cr BottomSide (x, y) d =
        R.Rectangle
            (x |+| (R.left $ cr^.bottom),
             y |+| (R.top $ cr^.bottom))
            ((R.width $ cr^.bottom) |+| d,
             R.height $ cr^.bottom)
