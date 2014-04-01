module FuzzyTimings.TimeSlice (TimeSlice(..), 
                          tsDuration,
                          mkTimeSlice,
                          overlaps,
                          addSecs,
                          inTimeSlice,
                          intersectTimeSlice,
                          deleteTimeSlice,
                          cutTimeSlice) where

import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Time.Clock
import Data.Maybe
import Data.List


data TimeSlice a = TimeSlice {
    tsStart    :: LocalTime,
    tsEnd      :: LocalTime,
    tsValue    :: a
} deriving (Show)

instance Eq (TimeSlice a) where
    t1 == t2 = tsStart t1 == tsStart t2 
             && tsEnd t1 == tsEnd t2

instance Ord (TimeSlice a) where
    compare t1 t2 = compare (tsStart t1) (tsStart t2)

tsDuration :: TimeSlice a -> Int
tsDuration t = floor $ (localTimeToUTC utc $ tsEnd t)
                       `diffUTCTime`
                       (localTimeToUTC utc $ tsStart t)

addSecs :: Int -> LocalTime -> LocalTime
addSecs s lt = LocalTime {
        localDay = addDays (fromIntegral (days + extraDay)) 
                           (localDay lt),
        localTimeOfDay = dayFractionToTimeOfDay endTodFrac
    }
    where
        days = s `div` 86400
        rest = (fromIntegral (s `mod` 86400)) / 86400
        tod = localTimeOfDay lt
        endTodFrac' = (timeOfDayToDayFraction tod) + rest
        (extraDay, endTodFrac)
            | endTodFrac' >= 1 = (1, endTodFrac' - 1)
            | otherwise = (0, endTodFrac') 
       
mkTimeSlice :: LocalTime -> Int -> a -> TimeSlice a
mkTimeSlice start duration value  = TimeSlice {
        tsStart = start,
        tsEnd = addSecs duration start,
        tsValue = value
    }

inTimeSlice :: TimeSlice a -> LocalTime -> Bool
inTimeSlice ts lt = lt >= tsStart ts && lt < tsEnd ts
overlaps :: TimeSlice a -> TimeSlice b -> Bool
overlaps t1 t2 
   | tsStart t1 == tsEnd t1 = False
   | tsStart t2 == tsEnd t2 = False
   | otherwise = (tsStart t1 >= tsStart t2 && tsStart t1 < tsEnd t2)
                 || (tsStart t2 >= tsStart t1 && tsStart t2 < tsEnd t1)

intersectTimeSlice :: TimeSlice a -> TimeSlice b -> Maybe (TimeSlice a)
intersectTimeSlice t1 t2
    | overlaps t1 t2 = Just (t1 {
            tsStart = max (tsStart t1) (tsStart t2),
            tsEnd = min (tsEnd t1) (tsEnd t2)
        })
    | otherwise = Nothing

deleteTimeSlice :: TimeSlice a -> TimeSlice b -> [TimeSlice a]
deleteTimeSlice t1 t2 = deleteBy $ intersectTimeSlice t1 t2
    where
        deleteBy (Just t3) 
            | tsStart t3 > tsStart t1 && tsEnd t3 < tsEnd t1 = 
                [ t1 { tsEnd = tsStart t3 }, t1 { tsStart = tsEnd t3 } ]
            | tsStart t3 > tsStart t1 = [ t1 { tsEnd = tsStart t3 }]
            | otherwise = [ t1 { tsStart = tsEnd t3 } ]
        deleteBy Nothing = [t1]
        

cutTimeSlice :: [LocalTime] -> TimeSlice a -> [TimeSlice a]
cutTimeSlice times ts = let
    times' = tsStart ts : filter (inTimeSlice ts) times ++ [tsEnd ts]
    cut (t1:t2:lts) = ts {
            tsStart = t1,
            tsEnd = t2
        }:cut (t2:lts)
    cut _ = []
    in (cut . sort . nub) times'


