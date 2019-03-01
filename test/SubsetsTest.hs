module SubsetsTest
    ( indexSetsEqualMapIdAsc
    , indexSetsEqualMapIdDesc
    , indexSetsEqualSubsetsAsc
    , indexSetsEqualSubsetsDesc
    ) where

import Data.Set.Subsets
import Control.Applicative
import qualified Data.Set as Set

mkPropIndexSetsEqualMapId :: (Int -> Int -> [Set.Set Int]) -> ((Int -> Int) -> Int -> Int -> [[Int]]) -> ((Int, Int) -> Bool)
mkPropIndexSetsEqualMapId indexSetsMkr mapper =
    (==) <$> ((map Set.toList) . (uncurry indexSetsMkr)) <*> (uncurry $ mapper id)

indexSetsEqualMapIdAsc :: (Int, Int) -> Bool
indexSetsEqualMapIdAsc = mkPropIndexSetsEqualMapId indexSetsAsc mapIndexSetsAsc

indexSetsEqualMapIdDesc :: (Int, Int) -> Bool
indexSetsEqualMapIdDesc = mkPropIndexSetsEqualMapId indexSetsDesc mapIndexSetsDesc

mkPropIndexSetsEqualSubsets :: (Int -> Int -> [Set.Set Int]) -> ([Int] -> Int -> Int -> [[Int]]) -> ((Int, Int) -> Bool)
mkPropIndexSetsEqualSubsets indexSetsMkr subsetMkr =
    (==) <$> ((map Set.toList) . (uncurry indexSetsMkr)) <*> (uncurry $ \m n -> subsetMkr [0..n-1] m n)

indexSetsEqualSubsetsAsc :: (Int, Int) -> Bool
indexSetsEqualSubsetsAsc = mkPropIndexSetsEqualSubsets indexSetsAsc subsetsAsc

indexSetsEqualSubsetsDesc :: (Int, Int) -> Bool
indexSetsEqualSubsetsDesc = mkPropIndexSetsEqualSubsets indexSetsDesc subsetsDesc
