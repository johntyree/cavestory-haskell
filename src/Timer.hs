{-# LANGUAGE TemplateHaskell #-}
module Timer ( Timer
             , makeTimer
             , reset
             , update
             , active
             , expired
             , currentTime
             ) where

import Control.Lens ( makeLenses
                    , (.~)
                    , (^.)
                    , (%~)
                    , view
                    )
import Units ( Time(..)
             , Unit(..)
             )

data Timer = Timer { _expiration :: !Time
                   , _current :: !Time
                   }
    deriving Show
makeLenses ''Timer

makeTimer :: Time -> Bool -> Timer
makeTimer expir isActive = Timer expir curr
  where
    curr
        | isActive = MS 0
        | otherwise = expir

update :: Time -> Timer -> Timer
update t timer = if active timer
                 then current %~ (t |+|) $ timer
                 else timer

reset :: Timer -> Timer
reset = current.~MS 0

active :: Timer -> Bool
active t = t^.current < t^.expiration

expired :: Timer -> Bool
expired = not . active

currentTime :: Timer -> Time
currentTime = view current
