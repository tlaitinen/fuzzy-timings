module FuzzyTimings.SlicedTime (SlicedTime, 
                           fromTimeSlices, 
                           toTimeSlices,
                           fromBoundaries,
                           flattenSlicedTime,
                           intersectSlicedTime,
                           deleteSlicedTime,
                           slicedTimeBoundaries,
                           cutSlicedTime,
                           mapSlicedTime) where
import Timings.TimeSlice
import Data.Maybe
import Control.Monad
import Data.Time.LocalTime
import Data.List

data SlicedTime a = SlicedTime {
        stSlices :: [TimeSlice a]
    } deriving (Show, Eq)


fromTimeSlices :: [TimeSlice a] -> SlicedTime a 
fromTimeSlices tss = SlicedTime {
        stSlices = tss
    }

toTimeSlices :: SlicedTime a -> [TimeSlice a]
toTimeSlices st = stSlices st

sortNub :: Ord a => [a] -> [a]
sortNub = map head . group . sort

fromBoundaries :: [LocalTime] -> a -> SlicedTime a
fromBoundaries bs d = SlicedTime {
        stSlices = slices bs
    }
    where slices (b1:b2:bs') = TimeSlice {
            tsStart = b1,
            tsEnd = b2,
            tsValue = d
                }:slices (b2:bs')
          slices _ = []



intersectSlicedTime :: SlicedTime a -> SlicedTime b -> SlicedTime a
intersectSlicedTime st1 st2 = st1 {
        stSlices = concatMap intersect (stSlices st1)
    }
    where intersect ts = mapMaybe (intersectTimeSlice ts) $ stSlices st2


deleteSlicedTime :: SlicedTime a -> SlicedTime b -> SlicedTime a
deleteSlicedTime st1 st2 = foldl delete st1 (stSlices st2)
    where delete st ts = st {
            stSlices = concatMap (\t -> deleteTimeSlice t ts) $ stSlices st
        }

flattenSlicedTime :: SlicedTime a -> SlicedTime a
flattenSlicedTime st = SlicedTime { 
        stSlices = flatten (sort . stSlices $ st)
    }
    where
        flatten (t1:t2:tss) 
            | overlaps t1 t2 = t1 { tsEnd = tsStart t2 } : flatten (t2:tss)
            | otherwise = t1 : flatten (t2:tss)
        flatten tss = tss

slicedTimeBoundaries :: SlicedTime a -> [LocalTime]
slicedTimeBoundaries st = concatMap (\ts -> [tsStart ts, tsEnd ts]) 
                                    (stSlices st)
    
cutSlicedTime :: SlicedTime a -> [LocalTime] -> SlicedTime a
cutSlicedTime st boundaries = st {
        stSlices = concatMap (cutTimeSlice boundaries) (stSlices st)
    }

mapSlicedTime :: SlicedTime a -> (TimeSlice a -> Maybe (TimeSlice a)) -> SlicedTime a
mapSlicedTime st op = st {
        stSlices = mapMaybe op (stSlices st)
    }
