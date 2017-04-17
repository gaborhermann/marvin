module Marvin.API.Algorithms.KMeansSpec where

import Test.Hspec
import Test.QuickCheck hiding (vector)

import Marvin.API.Algorithms.KMeans
import Marvin.Test.TestUtils
import Marvin.API

spec :: Spec
spec = describe "KMeans algorithm" $ do
        it "works on test data" $
          fit kmeans ps `shouldSatisfy` \m -> case m of
              Right model -> centroids model `isAround` expected
              Left _ -> False
        it "gives right distance for test data" $
          (do
            model <- fit kmeans ps
            test <- fromRows [[1.1, 2.1, 1.1]]
            toList <$> distancesFromNearestCentroids model test) `shouldSatisfy` \x -> case x of
              Right c -> c `isAround` [1.0]
              Left _ -> False
        it "gives error on empty table" $
          fit kmeans (emptyTable :: NumericTable) `shouldSatisfy` \m -> case m of
            Left (EmptyTable _) -> True
            Right _ -> False
        it "gives error on empty at prediction" $
          let Right model = fit kmeans ps in
            predict model (emptyTable :: NumericTable) `shouldSatisfy` \m -> case m of
              Left (RowLengthMismatch _) -> True
              Right _ -> False
        where
          ps :: NumericTable
          Right ps = fromRows $ map (\x -> [x,x,x]) [1.0, 1.1, 1.2, 9.0, 9.1, 9.2]
          kmeans = KMeans { k = 2, numberOfIterations = 100 }
          expected = [[1.1,1.1,1.1], [9.1,9.1,9.1]]

