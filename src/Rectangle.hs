module Rectangle ( Rectangle(..)
                 , left
                 , top
                 , right
                 , bottom
                 , width
                 , height
                 ) where

import Units ( Position
             , Dimension
             , Length(..)
             , Unit(..)
             )

data Rectangle = Rectangle Position Dimension

left :: Rectangle -> Length
left (Rectangle (x, _) _) = x

top :: Rectangle -> Length
top (Rectangle (_, y) _) = y

width :: Rectangle -> Length
width (Rectangle _ (w, _)) = w

height :: Rectangle -> Length
height (Rectangle _ (_, h)) = h

right :: Rectangle -> Length
right r = left r |+| width r

bottom :: Rectangle -> Length
bottom r = top r |+| height r
