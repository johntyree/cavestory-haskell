module SpriteState
    ( HorizontalFacing(..)
    , VerticalFacing(..)
    ) where

data HorizontalFacing = HorizLeft | HorizRight
    deriving (Eq, Enum, Bounded, Ord)
data VerticalFacing = VerticalNone | VerticalUp | VerticalDown
    deriving (Eq, Enum, Bounded, Ord)
