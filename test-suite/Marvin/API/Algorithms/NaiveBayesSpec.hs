module Marvin.API.Algorithms.NaiveBayesSpec where

import Test.Hspec

import Marvin.API.Algorithms.NaiveBayes
import Marvin.API

spec :: Spec
spec = describe "Naive Bayes fitting for smoothingValue = 0" $ do
  it "computes conditional probabilities for true target" $
     trueProbabilities model `shouldBe` expectedTrue
  it "computes conditional probabilities for false target" $
     falseProbabilities model `shouldBe` expectedFalse
  it "computes probability of true target" $
     probabilityOfTrue model `shouldBe` expectedProbOfTrue
  it "can give predictions" $
     predict model testX `shouldBe` fromList [True]
  it "gives error on fit with wrong dimensions" $
    fit testNB (x, yWrong) `shouldSatisfy` \m -> case m of
      Left (ColumnSizeMismatch _) -> True
      _ -> False
  it "gives error on prediction from wrong size" $
    predict model testXWrong `shouldSatisfy` \m -> case m of
        Left (RowLengthMismatch _) -> True
        _ -> False

testNB = NaiveBayes { smoothingValue = 0 }
Right model = fit testNB (x, y)
Right y = fromList $ intsToBools [0,1,1,1,1,0,1]
Right yWrong = fromList $ intsToBools [0,1,1,1,1,0]
Right x = fromRows $ map intsToBools [
      [0,0,1]
    , [1,0,1]
    , [0,0,0]
    , [0,1,1]
    , [1,0,0]
    , [1,1,1]
    , [0,1,0]
  ]
Right testX = fromRows $ map intsToBools [[0,0,1]]
Right testXWrong = fromRows $ map intsToBools [[0,0,1,0]]
expectedFalse = [0.5,0.5,1]
expectedTrue = [0.4,0.4,0.4]
expectedProbOfTrue = 5 / 7

intsToBools = map (\x -> if (x == 1) then True else False)
