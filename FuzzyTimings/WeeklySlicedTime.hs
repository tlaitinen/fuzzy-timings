module FuzzyTimings.WeeklySlicedTime (WeeklySlicedTime, 
                           fromTimeOfDaySlices, 
                           toTimeOfDaySlices,
                           flattenWeeklySlicedTime,
                           implementWeeklySlicedTime) where
import FuzzyTimings.TimeSlice
import FuzzyTimings.TimeOfDaySlice
import FuzzyTimings.SlicedTime
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Calendar
import Data.Maybe
import Data.Time.LocalTime
import Data.List
import qualified Data.Map as Map

type WeekDay = Int
type WSTMap a = Map.Map WeekDay [TimeOfDaySlice a]

data WeeklySlicedTime a = WeeklySlicedTime {
        wstSlices :: WSTMap a
    } deriving (Show, Eq)


fromTimeOfDaySlices :: [(WeekDay, [TimeOfDaySlice a])] -> WeeklySlicedTime a 
fromTimeOfDaySlices tss = WeeklySlicedTime {
        wstSlices = Map.fromList tss
    }

toTimeOfDaySlices :: WeeklySlicedTime a -> [(WeekDay, [TimeOfDaySlice a])]
toTimeOfDaySlices = Map.toList . wstSlices

flattenWeeklySlicedTime :: WeeklySlicedTime a -> WeeklySlicedTime a
flattenWeeklySlicedTime wst = WeeklySlicedTime { 
        wstSlices = Map.map (flatten . sort) (wstSlices wst)
    }
    where
        flatten (t1:t2:tss)
            --          |----|
            --            |----|
            | todsStart t1 < todsStart t2 && todsStart t2 < todsEnd t1
                                      && todsEnd t2 > todsEnd t1 =
                    if todsPriority t1 > todsPriority t2
                        then t1 : flatten (t2 { todsStart = todsEnd t1 } : tss)
                        else (t1 { todsEnd = todsStart t2 }) : flatten (t2:tss)
            --          |----|
            --            |--|                
            | todsStart t1 < todsStart t2 && todsEnd t1 == todsEnd t2 =
                    if todsPriority t1 > todsPriority t2
                        then flatten (t1:tss)
                        else flatten ((t1 { todsEnd = todsStart t2 }) : t2:tss)
            --          |----|
            --           |--|
            | todsStart t1 < todsStart t2 && todsEnd t2 < todsEnd t1 =
                    if todsPriority t1 > todsPriority t2
                        then flatten (t1:tss)
                        else flatten $ (t1 { todsEnd = todsStart t2})
                                      : (t2:(t1 { todsStart=todsEnd t2 }):tss)
            --          |----|
            --          |-------|                    
            | todsStart t1 == todsStart t2 && todsEnd t2 > todsEnd t1 =
                    if todsPriority t1 > todsPriority t2
                        then flatten (t1:t2 { todsStart = todsEnd t1 } : tss)
                        else flatten (t2:tss)
            --          |----|
            --          |----|
            | todsStart t1 == todsStart t2 && todsEnd t1 == todsEnd t2 =
                    if todsPriority t1 > todsPriority t2
                        then flatten (t1:tss)
                        else flatten (t2:tss)
            --          |----|
            --          |---|
            | todsStart t1 == todsStart t2 && todsEnd t1 > todsEnd t2 =
                    if todsPriority t1 > todsPriority t2
                        then flatten (t1:tss)
                        else flatten (t2:(t1 { todsStart = todsEnd t2 }):tss)
            | otherwise = t1 : flatten (t2:tss)
        flatten tss = tss
implementWeeklySlicedTime :: WeeklySlicedTime a -> Day -> Day -> SlicedTime a
implementWeeklySlicedTime wst d1 d2 = fromTimeSlices slices
    where   
        slices = [ TimeSlice (LocalTime d (todsStart tods))
                             (LocalTime d (todsEnd tods))
                             (todsPriority tods)
                             (todsValue tods)
                   | d <- days, tods <- Map.findWithDefault [] (weekDay d) 
                                                            (wstSlices wst) ]
        weekDay d = let (_,_,wd) = toWeekDate d in wd
        days = [ addDays i d1 | i <- [0..diffDays d2 d1] ]

