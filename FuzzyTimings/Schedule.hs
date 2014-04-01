module FuzzyTimings.Schedule (scheduleTimings) where
import FuzzyTimings.TimingBuckets
import FuzzyTimings.TimeSlice
import FuzzyTimings.SlicedTime
import FuzzyTimings.AccTiming
import FuzzyTimings.FuzzyTiming
import qualified Data.Map as Map
import System.Random
import Data.List

scheduleTimings :: Show k =>  SlicedTime (FuzzyCountMap k) -> IO [AccTiming k]
scheduleTimings st = do
    accTimings <- mapM scheduleSlice $ toTimeSlices st
    return $ concat accTimings

scheduleSlice :: Show k => TimeSlice (FuzzyCountMap k) -> IO [AccTiming k]
scheduleSlice ts = do
    print instanceRanges
    instances <- mapM rndInstancePos instanceRanges
    return [ AccTiming (ftId ft) (addSecs pos (tsStart ts)) (ftDuration ft)
           | (ft,pos) <- schedule instances ]
    where
        schedule instances = let 
            sortedInstances = sortBy (\(_,pos1) (_,pos2) -> compare pos1 pos2)
                                     instances
            in scheduleInstances 0 [ (ft,pos i (length instances)) 
                                   | ((ft,_), i) <- zip sortedInstances [0..] ]
        instanceRanges = [ (ft,pos (i-1) ttp, (pos (i-1) ttp + pos i ttp) `div` 2)  
                         | (ft,ttp) <- fts, i <- [1..ttp] ]
        pos i ttp = (tsDuration ts * i) `div` ttp
        fts = [ (ft,floor ttp) | (ft,ttp) <- Map.toList (tsValue ts) ]
        rndInstancePos (ft,start,end) = do
            pos <- randomRIO (start,end)
            return (ft,pos)

        
scheduleInstances :: Int -> [(FuzzyTiming k, Int)] -> [(FuzzyTiming k, Int)]
scheduleInstances now ((ft,pos):xs) 
    | now > pos = (ft,now):scheduleInstances (now+ftDuration ft) xs
    | otherwise = (ft,pos):scheduleInstances (pos+ftDuration ft) xs
scheduleInstances _ [] = []
                           
                                    
                
    


