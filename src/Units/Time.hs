module Units.Time ( Time(..)
                  , asMS
                  , MSType
                  , SType
                  , deltaT
                  ) where

import Data.Word ( Word32 )

type MSType = Word32
type SType = Word32
data Time = MS MSType |
            S SType
    deriving Show

instance Eq Time where
    t1 == t2 = (asMS t1) == (asMS t2)
instance Ord Time where
    t1 `compare` t2 = (asMS t1) `compare` (asMS t2)

asMS :: Time -> MSType
asMS (MS ms) = ms
asMS (S s) = s * 1000

deltaT :: Time -> Time -> Time
deltaT before after = MS $ (asMS after) - (asMS before)
