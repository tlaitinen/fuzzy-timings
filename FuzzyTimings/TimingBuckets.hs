module FuzzyTimings.TimingBuckets (FuzzyCountMap,
                              TimingBuckets,
                              splitToTimingBuckets)
                             where

import FuzzyTimings.SlicedTime
import FuzzyTimings.TimeSlice
import FuzzyTimings.FuzzyTiming
import FuzzyTimings.AccTiming
import Data.Time.LocalTime
import Data.Maybe
import Data.List
import qualified Data.Map as Map
import System.Random
import Control.Monad.State
type FuzzyCountMap k = Map.Map (FuzzyTiming k) TimesToPlay
type TimingBuckets k = SlicedTime (FuzzyCountMap k)

emptyBucket :: FuzzyCountMap k
emptyBucket = Map.empty

addToBucketGoal :: Ord k => FuzzyCountMap k -> FuzzyCountMap k -> FuzzyCountMap k
addToBucketGoal = Map.unionWith (+)

emptyBuckets :: [LocalTime] -> TimingBuckets k
emptyBuckets lts = fromBoundaries (nub lts) emptyBucket

cropBuckets :: (Ord k) => TimingBuckets k -> SlicedTime b -> TimingBuckets k
cropBuckets tb st = deleteSlicedTime tb st

splitToBuckets :: (Ord k) => TimingBuckets k -> FuzzyTiming k -> TimingBuckets k
splitToBuckets tb ft = mapSlicedTime tb splitTo
    where 
        playTimes = toTimeSlices $ ftPlayTimes ft
        splitTo t = Just $ t {
                tsValue = addToBucketGoal (tsValue t) 
                             (Map.fromList (mapMaybe (proportion t) playTimes))
        }
        proportion t1 t2 = do
            t3 <- intersectTimeSlice t1 t2
            return $ (ft, tsValue t2 * fromIntegral (tsDuration t3) / 
                                       fromIntegral (tsDuration t2))
                               
splitToTimingBuckets :: (Ord k) => [FuzzyTiming k] -> [AccTiming k] -> TimingBuckets k
splitToTimingBuckets fts ats = let
    boundaries = concatMap (slicedTimeBoundaries . ftPlayTimes) fts
    reserved = fromTimeSlices $ [ mkTimeSlice (atTime at) (atDuration at) ()
                                  | at <- ats ]
    in foldl (\tb ft -> splitToBuckets tb ft) 
             (cropBuckets (emptyBuckets boundaries)
                          reserved)
             fts


