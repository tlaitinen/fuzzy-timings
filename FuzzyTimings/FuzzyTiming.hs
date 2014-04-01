module FuzzyTimings.FuzzyTiming (FuzzyTiming(..), 
                            ftPlayCount,
                            cropFuzzyTiming,
                            TimesToPlay) where

import FuzzyTimings.SlicedTime
import FuzzyTimings.TimeSlice

type TimesToPlay = Double

-- | "Fuzzily" timed object that is to be scheduled a number of times within a period of time. 
data FuzzyTiming k = FuzzyTiming {
    ftId         :: k,
    ftPlayTimes  :: SlicedTime TimesToPlay,
    ftDuration   :: Int
} deriving (Show)

instance (Eq k) => Eq (FuzzyTiming k) where
    ft1 == ft2 = ftId ft1 == ftId ft2

instance (Ord k) => Ord (FuzzyTiming k) where
    compare f1 f2 = compare (ftId f1) (ftId f2)

ftPlayCount :: FuzzyTiming k -> Double
ftPlayCount ft = sum [ tsValue ts | ts <- toTimeSlices $ ftPlayTimes ft ]

cropFuzzyTiming :: SlicedTime () -> FuzzyTiming k -> FuzzyTiming k
cropFuzzyTiming allowed ft = ft {
        ftPlayTimes = mapSlicedTime (intersectSlicedTime (ftPlayTimes ft) allowed) adjustTimesToPlay

    }
    where 
        adjustTimesToPlay ts = Just $ ts {
            tsValue = (fromIntegral $ tsDuration ts) 
                      * totalTimesToPlay / (fromIntegral totalLength)
        }
        playTimeSlices = toTimeSlices $ ftPlayTimes ft
        totalTimesToPlay = sum $ map tsValue playTimeSlices
        totalLength      = sum $ map tsDuration playTimeSlices

        
