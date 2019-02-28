-- | Module: Data.Set.Subsets.BankersSequence
-- Copyright: 2019 Felix Linker
-- License: MIT
-- Maintainer: linkerfelix@gmail.com
--
-- This module generates the Banker's sequence which is an efficient enumeration
-- for generating subsets in ascending (respective to their subset-order) or
-- descending order.
--
-- The idea of the Banker's sequence is described in
-- <https://www.researchgate.net/publication/2526315_Efficiently_Enumerating_the_Subsets_of_a_Set>
-- but in this module no implementation details of this paper are used.
--
-- In this module, often times so called /index representations/ of ints are
-- used.
-- An index representation of an @ Int @ is a list that holds all indices
-- (starting from 0) which are 1 in the binary representation.
-- For example the index representation of @ 5 @ is @ [0, 2] @ because its
-- binary representation is @ 101 @.
module Data.Set.Subsets.BankersSequence
    ( bankersSequence
    , invBankersSequence
    , bankersSequenceLists
    , invBankersSequenceLists
    ) where

-- | /O(2^n*n)/ where /n/ is the input __value__. Generate the Banker's sequence.
bankersSequence :: Int -> [Int]
bankersSequence = map listToInt . bankersSequenceLists 0

-- | Same as 'bankersSequence' but in inverted order.
invBankersSequence :: Int -> [Int]
invBankersSequence = map listToInt . invBankersSequenceLists 0

listToInt :: [Int] -> Int
listToInt = sum . (map (2^))

bankersSequenceListsWith :: (Maybe [Int] -> Maybe [Int]) -> Maybe [Int] -> [[Int]]
bankersSequenceListsWith step initial = case step initial of
    Just l -> l : (bankersSequenceListsWith step $ Just l)
    Nothing -> []

-- | /O(2^n*n)/.
-- Generate the Banker's sequence as index representations starting at
-- representations of length /m/ up to a maximum length of /n/.
bankersSequenceLists :: Int -> Int -> [[Int]]
bankersSequenceLists from maxLen =
    let initial = [0..from - 1]
    in initial : bankersSequenceListsWith (next maxLen) (Just initial)

-- | Same as 'bankersSequenceLists' but in inverted order starting at length /m/
-- going to 0 for a maximum length of /n/.
--
-- If the first argument is smaller than 0, all index representations will be
-- generated.
invBankersSequenceLists :: Int -> Int -> [[Int]]
invBankersSequenceLists from maxLen
    | from < 0 = invBankersSequenceLists maxLen maxLen
    | otherwise = let initial = [0..from - 1]
        in initial : bankersSequenceListsWith (prev maxLen) (Just initial)

-- | /O(n)/.
-- Try to generate the next index representation in the Banker's sequence a given
-- maximum value /n/ and a preceeding index representation.
next :: Int -> Maybe [Int] -> Maybe [Int]
next = (=<<) . (tryIncrement 0)

tryIncrement :: Int -> Int -> [Int] -> Maybe [Int]
tryIncrement read maxLen []
    | maxLen <= 0 = Nothing
    | otherwise = Just [read]
tryIncrement read maxLen (most:[])
    | most == maxLen - 1 =
        let read' = read + 1
        in if read' < maxLen then Just [read, read'] else Nothing
    | otherwise = Just [most + 1]
tryIncrement read maxLen (most:most':mosts)
    | most + 1 < most' = Just $ most + 1 : most' : mosts
    | otherwise = fmap (read:) $ tryIncrement (read + 1) maxLen (most':mosts)

-- | /O(n)/.
-- Try to generate the previous index representation in the bankers sequence for
-- a given maximum value /n/ and a given proceeding index representation.
prev :: Int -> Maybe [Int] -> Maybe [Int]
prev = (=<<) . (tryDecrement 0)

tryDecrement :: Int -> Int -> [Int] -> Maybe [Int]
tryDecrement _ _ [] = Nothing
tryDecrement _ _ (0:[]) = Just []
tryDecrement read maxLen (most:[])
    | read == most = Just [maxLen - read..maxLen - 1]
    | otherwise = Just [most - 1 - read..most - 1]
tryDecrement read maxLen (most:most':mosts)
    | read < most = Just $ [most - 1 - read..most - 2] ++ (most - 1 : most' : mosts)
    | otherwise = tryDecrement (read + 1) maxLen $ most' : mosts
