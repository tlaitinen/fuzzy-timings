module FuzzyTimings.Solve (solveTimingBuckets) where

import FuzzyTimings.TimingBuckets
import FuzzyTimings.SlicedTime
import FuzzyTimings.TimeSlice
import FuzzyTimings.FuzzyTiming
import FuzzyTimings.AccTiming
import Control.Monad.LPMonad
import Control.Monad
import Data.LinearProgram
import qualified Data.Map as Map

-- tVar corresponds to the combined play counts for a single fuzzy timing
tVar :: (Show k, Ord k) => FuzzyTiming k -> String
tVar f = "p" ++ (show $ ftId f)

-- bfVar corresponds to the play counts for a fuzzy timing in a single timing
-- bucket
bfVar :: (Show k, Ord k) => (Int, FuzzyTiming k) -> String
bfVar (i,f) = "b" ++ (show i) ++ "_" ++ (show $ ftId f)

-- sVar corresponds to the number of seconds used to play spots in a single
-- timing bucket
sVar :: Int -> String
sVar i = "s" ++ (show i)

-- the objective is maximize the amount of spot seconds to play
-- consisting of combined play counts of each FuzzyTiming
objFun :: (Show k, Ord k) => [FuzzyTiming k] -> LinFunc String Int
objFun fuzzies = linCombination $ [(ftDuration f, tVar f) | f <- fuzzies ]

timingBucketsLp :: (Show k, Ord k) => SlicedTime (FuzzyCountMap k) -> LP String Int
timingBucketsLp st = execLPM $ do
    setDirection Max
    setObjective (objFun fuzzies)
    forM_ (Map.assocs tCounts) (\(f,c) -> do
        setVarKind (tVar f) IntVar
        -- the total play count must be smaller than equal to the
        -- desired play count
        varBds (tVar f) 0 (ceiling c)
        -- the total play count consists of play counts in individual buckets
        equal (var (tVar f)) $ varSum [ bfVar (i,f') | (i,ts) <- nSlices,
                                           (f',c') <- Map.assocs $ tsValue ts,
                                           f == f']
        )
    forM_ nSlices (\(i,ts) -> do
        -- measure the number of seconds play spots in this bucket
        equal (var (sVar i)) $ linCombination [ (ftDuration f, bfVar (i,f))
                                              | f <- Map.keys $ tsValue ts  ]
        -- only 75% of the seconds can be used to play spots
        varBds (sVar i) 0 (floor $ (0.75::Double) * (fromIntegral $ tsDuration ts))
        forM_ (Map.assocs $ tsValue ts) (\(f,c) -> do
            setVarKind (bfVar (i,f)) IntVar
            -- allow to play one additional time in case of fractional amounts
            -- in a single bucket 
            varBds (bfVar (i,f)) 0 (ceiling c)
            ))       
    where
        slices = toTimeSlices st
        fCounts = map tsValue slices
        nSlices = [ (i, ts) | (i,ts) <- zip [1..] slices ]
        -- combined play counts for all fuzzies
        tCounts = foldl (Map.unionWith (+)) Map.empty fCounts
        fuzzies = Map.keys tCounts

updateTimingBuckets :: (Show k, Ord k) => SlicedTime (FuzzyCountMap k) -> Map.Map String Double -> SlicedTime (FuzzyCountMap k) 
updateTimingBuckets st vm = fromTimeSlices [ ts {
            tsValue = Map.mapWithKey (updateTs i) $ tsValue ts 
        } | (i, ts) <- zip [1..] (toTimeSlices st) ]
    where
        updateTs i f _ = Map.findWithDefault 0.0 (bfVar (i,f)) vm

solveTimingBuckets :: (Show k, Ord k) => SlicedTime (FuzzyCountMap k) -> IO (Maybe (SlicedTime (FuzzyCountMap k)))
solveTimingBuckets st = do
    let lp = timingBucketsLp st
    print $ lp
    (_, mresult) <- glpSolveVars mipDefaults lp
    print mresult
    let res =  (mresult >>= \(_,vm) -> return $ updateTimingBuckets st vm)
    print res
    return res

