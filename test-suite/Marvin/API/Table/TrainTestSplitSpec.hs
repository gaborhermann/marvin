module Marvin.API.Table.TrainTestSplitSpec where

import Test.Hspec
import Test.QuickCheck

import System.Random
import Data.List (sort)

import Marvin.API
import Marvin.API.Table.TrainTestSplit

import Marvin.Test.TestUtils

spec :: Spec
spec =
  describe "Splitting data to train and test" $ do
    it "the union is the same after the split" $
      property $ \numCol g -> (do
        tab <- fromColumns [numCol] :: Fallible NumericTable
        let gen = mkStdGen g :: StdGen
        (train, test) <- trainTestSplitRandom gen 0.7 tab
        let mkList = toList . head . columns
        return $ mkList train ++ mkList test) `shouldSatisfy` \x -> case x of
          Left EmptyColumn -> True
          Right unionList -> sort unionList == sort (toList numCol)
          _ -> False
