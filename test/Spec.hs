module Main where

import Data.Set.Subsets
import Test.QuickCheck
import Control.Applicative
import BankersSequenceTest

main :: IO ()
main = do
    quickCheck $ forAll n bankersEqualsIndexSetsAsc
    quickCheck $ forAll n bankersEqualsIndexSetsDesc
    where n = choose (0, 10) :: Gen Int
