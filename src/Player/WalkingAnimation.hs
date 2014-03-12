{-# LANGUAGE TemplateHaskell #-}
module Player.WalkingAnimation ( Stride(..)
                               , WalkingAnimation
                               , makeWalkingAnimation
                               , update
                               , reset
                               , stride
                               ) where

import Control.Lens ( makeLenses
                    , view
                    , (.~)
                    , (^.)
                    , (%~)
                    )
import qualified Timer as T
import Units ( FrameRate(..)
             , Time(..)
             , asTimePerFrame
             )

data Stride = StrideMiddle | StrideLeft | StrideRight
    deriving (Eq, Enum, Bounded, Ord, Show)

data WalkingAnimation = WalkingAnimation { _current :: !Stride
                                         , _timer :: !T.Timer
                                         , _forward :: !Bool
                                         }
    deriving Show
makeLenses ''WalkingAnimation

frameRate :: FrameRate
frameRate = FrameRate 15 (S 1)

makeWalkingAnimation :: WalkingAnimation
makeWalkingAnimation = WalkingAnimation
    StrideMiddle
    (T.makeTimer (asTimePerFrame frameRate) True)
    True

update :: Time -> WalkingAnimation -> WalkingAnimation
update dt = checkExpiration . updateTimer
  where
    updateTimer, checkExpiration, onExpiration,
        incrFrame, decrFrame :: WalkingAnimation -> WalkingAnimation

    updateTimer = timer %~ (T.update dt)
    checkExpiration wa = if T.expired $ wa^.timer
                         then onExpiration . (timer %~ T.reset) $ wa
                         else wa
    onExpiration wa
        | wa^.forward = incrFrame wa
        | otherwise = decrFrame wa

    incrFrame = boundCheck . (current %~ succ)
      where
        boundCheck wa = if wa^.current == maxBound
                        then forward.~False $ wa
                        else wa
    decrFrame = boundCheck . (current %~ pred)
      where
        boundCheck wa = if wa^.current == minBound
                        then forward.~True $ wa
                        else wa

reset :: WalkingAnimation -> WalkingAnimation
reset = undefined

stride :: WalkingAnimation -> Stride
stride = view current
