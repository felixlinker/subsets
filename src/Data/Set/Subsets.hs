-- | Module: Data.Set.Subsets
-- Description: Iterate over subsets efficiently
-- Copyright:
-- License:
-- Maintainer: felixlinker
--
-- This module provides methods to efficiently (especially when it comes to
-- memory) over all subsets of a given base set.
-- Each subset is generated only by looking at the preceeding subset which leads
-- to a constant memory consumption throughout an iteration.
-- Additionally, sets can be iterated respecting their subset-partial-order
-- which in some use cases can save performance.
--
-- This module is implemented using the "Data.Set.Subsets.BankersSequence"
-- module.
module Data.Set.Subsets
    ( subsetsAsc
    , subsetsDesc
    , indexSetsAsc
    , indexSetsDesc
    , mapIndexSetsAsc
    , mapIndexSetsDesc
    ) where

import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Set.Subsets.BankersSequence

-- | /O(2^n*n*log n)/ which means that each subset is generated in /O(n*log n)/.
--
-- Take a list and treat it as a set where each index holds a unique element;
-- generate all subsets in ascending order starting with a given size /m/ up to
-- a base set size /n/.
--
-- In most use cases you'd want to set the third argument to the length of the
-- first.
-- Otherwise you'd just generate subsets for a prefix of length /n/ of that
-- list.
--
-- >>> subsetsAsc ['a', 'b', 'c'] 0 3
-- ["","a","b","c","ab","ac","bc","abc"]
subsetsAsc :: [a] -> Int -> Int -> [[a]]
subsetsAsc as =
    let map = Map.fromList $ indexes as
    in mapIndexSetsAsc (map Map.!)

-- | Same as @'subsetsAsc'@ but subsets will be generated descending starting
-- with all sets of size /n/ to /0/.
subsetsDesc :: [a] -> Int -> Int -> [[a]]
subsetsDesc as =
    let map = Map.fromList $ indexes as
    in mapIndexSetsDesc (map Map.!)

indexes :: [a] -> [(Int, a)]
indexes = zip [0..]

-- | /O(2^n*n)/ which means that each subset is generated in /O(n)/ - basically
-- without any overhead.
--
-- Generate all index-sets in ascending order starting with size /m/ up to /n/
-- elements.
--
-- An index-set is a set of 'Int's which are >= 0.
-- Mapping all index sets in @indexSetsAsc@ to a list of arbitrary but
-- distinct elements will select all subsets of that list in subset-ascending
-- order.
--
-- >>> indexSetsAsc 0 2
-- [fromList [], fromList [0], fromList [1], fromList [0, 1]]
--
-- >>> indexSetsAsc 1 2
-- [fromList [0], fromList [1], fromList [0, 1]]
indexSetsAsc :: Int -> Int -> [Set.Set Int]
indexSetsAsc = curry $ map Set.fromAscList . uncurry bankersSequenceLists

-- | Same as 'indexSetsAsc' but in descending order.
indexSetsDesc :: Int -> Int -> [Set.Set Int]
indexSetsDesc = curry $ map Set.fromAscList . uncurry invBankersSequenceLists

-- | /O(2^n*n*O(@f@))/ which means that each subset is generated basically
-- without any overhead besides @f@.
--
-- Generate index-sets in ascending order and map each index to the return
-- value of the function @f@ supplied.
--
-- >>> f x = if x <= 0 then 'a' else 'b'
-- >>> mapIndexSetsAsc f 0 2
-- ["", "a", "b", "ab"]
mapIndexSetsAsc :: (Int -> a) -> Int -> Int -> [[a]]
mapIndexSetsAsc f = curry $ map (map f) . uncurry bankersSequenceLists

-- | Same as 'mapIndexSetsAsc' but in descending order.
mapIndexSetsDesc :: (Int -> a) -> Int -> Int-> [[a]]
mapIndexSetsDesc f = curry $ map (map f) . uncurry invBankersSequenceLists
