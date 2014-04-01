module FuzzyTimings.AccTiming where

import Data.Time.LocalTime

data AccTiming k = AccTiming {
    atId         :: k,
    atTime       :: LocalTime,
    atDuration   :: Int
} deriving (Show)

instance (Eq k) => Eq (AccTiming k) where
    a1 == a2 = atId a1 == atId a2
