module Main where

import Test.QuickCheck
import Control.Applicative
import BankersSequenceTest
import SubsetsTest

main :: IO ()
main = do
    quickCheck $ forAll n bankersEqualsIndexSetsAsc
    quickCheck $ forAll n bankersEqualsIndexSetsDesc
    quickCheck $ forAll nTimesN indexSetsEqualMapIdAsc
    quickCheck $ forAll nTimesN indexSetsEqualMapIdDesc
    quickCheck $ forAll nTimesN indexSetsEqualSubsetsAsc
    quickCheck $ forAll nTimesN indexSetsEqualSubsetsDesc
    where
        n = choose (0, 10) :: Gen Int -- limit n for performance reasons
        nTimesN = (,) <$> (arbitrary :: Gen Int) <*> (n :: Gen Int)
