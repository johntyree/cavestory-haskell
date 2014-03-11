module Units.Time ( Time(..)
                  , asMS
                  , MSType
                  , SType
                  ) where

import Data.Word ( Word32 )

type MSType = Word32
type SType = Word32
data Time = MS MSType |
            S SType

asMS :: Time -> MSType
asMS (MS ms) = ms
asMS (S s) = s `div` 1000
