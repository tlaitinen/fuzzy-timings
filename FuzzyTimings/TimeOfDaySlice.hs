module FuzzyTimings.TimeOfDaySlice (TimeOfDaySlice(..),
                               mkTimeOfDaySlice,
                               mkPriorityTimeOfDaySlice,
                               todsOverlaps) where

import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Time.Clock
import Data.Maybe
import Data.List


data TimeOfDaySlice a = TimeOfDaySlice {
    todsStart  :: TimeOfDay,
    todsEnd    :: TimeOfDay,
    todsPriority :: Int,
    todsValue    :: a
} deriving (Show)


instance Eq (TimeOfDaySlice a) where
    t1 == t2 = todsStart t1 == todsStart t2 
             && todsEnd t1 == todsEnd t2

instance Ord (TimeOfDaySlice a) where
    compare t1 t2 = compare (todsStart t1) (todsStart t2)
 
mkTimeOfDaySlice :: TimeOfDay -> Int -> a -> TimeOfDaySlice a
mkTimeOfDaySlice start duration value  = TimeOfDaySlice {
        todsStart = start,
        todsEnd   = dayFractionToTimeOfDay $ 
                        timeOfDayToDayFraction start + (fromIntegral duration / 86400),
        todsPriority = 0,
        todsValue = value
    }
mkPriorityTimeOfDaySlice :: TimeOfDay -> Int -> Int -> a -> TimeOfDaySlice a
mkPriorityTimeOfDaySlice start duration priority value = (mkTimeOfDaySlice start duration value) {
        todsPriority = priority
    }


todsOverlaps :: TimeOfDaySlice a -> TimeOfDaySlice b -> Bool
todsOverlaps t1 t2 
   | todsStart t1 == todsEnd t1 = False
   | todsStart t2 == todsEnd t2 = False
   | otherwise = (todsStart t1 >= todsStart t2 && todsStart t1 < todsEnd t2)
                 || (todsStart t2 >= todsStart t1 && todsStart t2 < todsEnd t1)

    
