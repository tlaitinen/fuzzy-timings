import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.Framework.Providers.HUnit

import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.List
import Data.Maybe
import FuzzyTimings.TimeSlice
import FuzzyTimings.SlicedTime
import FuzzyTimings.FuzzyTiming
import FuzzyTimings.AccTiming
import FuzzyTimings.TimingBuckets
import FuzzyTimings.Solve
import FuzzyTimings.Schedule
import System.Random
import Control.Monad.State
import qualified Data.Map as Map

instance Arbitrary UTCTime where
     arbitrary       = do
         
         day <- choose (1, 28) :: Gen Int
         month <- choose (1, 12) :: Gen Int
         year <- choose (1970, 2030) :: Gen Integer
         seconds <- choose (0, 86400) :: Gen Integer

         return $ UTCTime { 
                utctDay = fromGregorian year month day,
                utctDayTime = secondsToDiffTime seconds
             }

instance Arbitrary LocalTime where
    arbitrary = do
        year <- choose (1970, 2030) :: Gen Integer
        month <- choose (1, 12) :: Gen Int
        day <- choose (1, gregorianMonthLength year month) :: Gen Int
        hour <- choose (0, 23) :: Gen Int
        min <- choose (0, 59) :: Gen Int
        sec <- choose (0, 59) :: Gen Int
        return $ LocalTime {
            localDay       = fromGregorian year month day,
            localTimeOfDay = TimeOfDay {
                todHour = hour,
                todMin = min,
                todSec = (fromIntegral sec)
            }
        }


instance Arbitrary a => Arbitrary (TimeSlice a) where
    arbitrary = do
        value <- arbitrary
        start <- arbitrary
        duration <- choose (0, 86400) :: Gen Int

        return $ mkTimeSlice start duration value
       
instance (Arbitrary a, Ord a, Show a) => Arbitrary (FuzzyTiming a) where
    arbitrary = do
        idNum <- arbitrary
        playTimes <- arbitrary
        times <- choose (0, 10) :: Gen Double
        duration <- choose (0, 300) :: Gen Int

        return $ FuzzyTiming {
            ftId = idNum,
            ftPlayTimes = fromTimeSlices playTimes,
            ftDuration = duration
        }

instance (Arbitrary a, Ord a, Show a) => Arbitrary (AccTiming a) where
    arbitrary = do
        idNum <- arbitrary
        time <- arbitrary
        duration <- choose (0, 300) :: Gen Int
        return $ AccTiming {
            atId   = idNum,
            atTime = time,
            atDuration = duration                  
        }



instance (Arbitrary a, Ord a, Show a) => Arbitrary (SlicedTime a) where
    arbitrary = do
        tss <- arbitrary
        return $ fromTimeSlices tss
occurrences :: Ord a => [a] -> [(a, Int)]
occurrences = map (\xs@(x:_) -> (x, length xs)) . group . sort

main :: IO ()
main = defaultMain tests

lt1 :: LocalTime
lt1 = LocalTime {
        localDay = fromGregorian 2013 1 1,
        localTimeOfDay = TimeOfDay {
            todHour = 18,
            todMin = 30,
            todSec = 15
        }
    }
lt2 :: LocalTime
lt2 = LocalTime {
        localDay = fromGregorian 2013 1 1,
        localTimeOfDay = TimeOfDay {
            todHour = 19,
            todMin = 45,
            todSec = 30
        }
    }

tsDuration_prop :: Bool
tsDuration_prop = tsDuration (TimeSlice { tsStart = lt1, tsEnd = lt2, tsValue = () }) == 4515

tsIntersect_prop :: TimeSlice () -> TimeSlice () -> Bool
tsIntersect_prop ts1 ts2 = overlaps ts1 ts2 == 
                              isJust (intersectTimeSlice ts1 ts2)

tsIntersect_commutative :: TimeSlice () -> TimeSlice () -> Bool
tsIntersect_commutative ts1 ts2 = intersectTimeSlice ts1 ts2 == intersectTimeSlice ts2 ts1
slicedTimeIntersectProp :: SlicedTime () -> SlicedTime () -> Bool
slicedTimeIntersectProp st1 st2 = intersectSlicedTime st1 st2 == intersectSlicedTime st2 st1

timingBucketsAccReserved :: [FuzzyTiming ()] -> [AccTiming ()] -> Bool
timingBucketsAccReserved fts ats = let
    tb = splitToTimingBuckets fts ats
    reserved = fromTimeSlices [ mkTimeSlice (atTime at) (atDuration at) () 
                                 | at <- ats ]
    commonSt = intersectSlicedTime reserved tb
    in toTimeSlices commonSt == []


cutTimeSlice_boundaries :: [LocalTime] -> TimeSlice () -> Bool
cutTimeSlice_boundaries lts ts = let
    tss = cutTimeSlice lts ts
    crossing ts = any (\lt -> lt > tsStart ts && lt < tsEnd ts) lts
    in all (not . crossing) tss

slicedTimeFlattenNoOverlaps :: SlicedTime () -> Bool
slicedTimeFlattenNoOverlaps st = let    
        tss = sort . toTimeSlices . flattenSlicedTime $ st
    in all id [ not $ overlaps  t1 t2 | t1 <- tss, t2 <- tss, t1 /= t2 ]
 

tests :: [Test.Framework.Test]
tests = [
        testGroup "FuzzyTimings.TimeSlice" $ [
            testProperty "tsDuration" tsDuration_prop,
            testProperty "cutTimeSlice_boundary_not_crossed" cutTimeSlice_boundaries,
            testProperty "intersect" tsIntersect_prop,
            testProperty "intersect-commutative" tsIntersect_commutative
        ],
        testGroup "FuzzyTimings.SlicedTime" $ [
            testProperty "intersect-commutative" slicedTimeIntersectProp,
            testProperty "flatten-no-overlaps" slicedTimeFlattenNoOverlaps

        ],
        testGroup "FuzzyTimings.TimingBuckets" $ [
            testProperty "accurate-reserved" timingBucketsAccReserved
        ]

    ] 
