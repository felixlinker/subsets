module BankersSequenceTest
    ( bankersEqualsIndexSetsAsc
    , bankersEqualsIndexSetsDesc
    ) where

import Test.QuickCheck
import Data.Set.Subsets
import Data.Set.Subsets.BankersSequence
import Control.Applicative
import qualified Data.Set as Set

indexSetsAsBinary :: [Set.Set Int] -> [Int]
indexSetsAsBinary = map (foldl (+) 0 . Set.map (2^))

mkPropBankersEqualsIndexSets :: (Int -> [Int]) -> (Int -> [Set.Set Int]) -> (Int -> Bool)
mkPropBankersEqualsIndexSets bankersMkr indexSetsMkr =
    (==) <$> bankersMkr <*> (indexSetsAsBinary . indexSetsMkr)

bankersEqualsIndexSetsAsc :: Int -> Property
bankersEqualsIndexSetsAsc = label "Index sets as binary index sets interpreted equal Banker's sequence (asc)" .
    (mkPropBankersEqualsIndexSets bankersSequence $ indexSetsAsc 0)

bankersEqualsIndexSetsDesc :: Int -> Property
bankersEqualsIndexSetsDesc = label "Index sets as binary index sets interpreted equal Banker's sequence (desc)" .
    (mkPropBankersEqualsIndexSets invBankersSequence $ indexSetsDesc (-1))
