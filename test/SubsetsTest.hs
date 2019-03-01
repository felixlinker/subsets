module SubsetsTest
    ( indexSetsEqualMapIdAsc
    , indexSetsEqualMapIdDesc
    , indexSetsEqualSubsetsAsc
    , indexSetsEqualSubsetsDesc
    ) where

import Test.QuickCheck
import Data.Set.Subsets
import Control.Applicative
import qualified Data.Set as Set

mkPropIndexSetsEqualMapId :: (Int -> Int -> [Set.Set Int]) -> ((Int -> Int) -> Int -> Int -> [[Int]]) -> ((Int, Int) -> Bool)
mkPropIndexSetsEqualMapId indexSetsMkr mapper =
    (==) <$> ((map Set.toList) . (uncurry indexSetsMkr)) <*> (uncurry $ mapper id)

indexSetsEqualMapIdAsc :: (Int, Int) -> Property
indexSetsEqualMapIdAsc = label "Index sets equal mapped sets with identity (asc)" .
    mkPropIndexSetsEqualMapId indexSetsAsc mapIndexSetsAsc

indexSetsEqualMapIdDesc :: (Int, Int) -> Property
indexSetsEqualMapIdDesc = label "Index sets equal mapped sets with identity (desc)" .
    mkPropIndexSetsEqualMapId indexSetsDesc mapIndexSetsDesc

mkPropIndexSetsEqualSubsets :: (Int -> Int -> [Set.Set Int]) -> ([Int] -> Int -> Int -> [[Int]]) -> ((Int, Int) -> Bool)
mkPropIndexSetsEqualSubsets indexSetsMkr subsetMkr =
    (==) <$> ((map Set.toList) . (uncurry indexSetsMkr)) <*> (uncurry $ \m n -> subsetMkr [0..n-1] m n)

indexSetsEqualSubsetsAsc :: (Int, Int) -> Property
indexSetsEqualSubsetsAsc = label "Index sets equal subsets of index lists (asc)" .
    mkPropIndexSetsEqualSubsets indexSetsAsc subsetsAsc

indexSetsEqualSubsetsDesc :: (Int, Int) -> Property
indexSetsEqualSubsetsDesc = label "Index sets equal subsets of index lists (desc)" .
    mkPropIndexSetsEqualSubsets indexSetsDesc subsetsDesc
