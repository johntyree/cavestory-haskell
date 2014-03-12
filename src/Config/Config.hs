module Config.Config ( GraphicsQuality(..)
                     , targetFrameTime
                     ) where

import Units.Time ( Time(..)
                  , asMS
                  )

data GraphicsQuality = OriginalQuality |
                       HighQuality
    deriving (Eq)

targetFps :: Int
targetFps = 60

targetFrameTime :: Time
targetFrameTime = MS $ (asMS $ S 1) `div` fromIntegral targetFps
