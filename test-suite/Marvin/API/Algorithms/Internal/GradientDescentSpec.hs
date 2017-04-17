module Marvin.API.Algorithms.Internal.GradientDescentSpec where

import Test.Hspec

import Marvin.API.Algorithms.Internal.GradientDescent hiding (learningRate)
import qualified Marvin.API.Algorithms.Internal.GradientDescent as GD

import Marvin.API.Fallible
import Marvin.API.Table.Internal
import Marvin.Test.TestUtils

import qualified Numeric.LinearAlgebra as LA

-- todo lambda, addIntercept
spec :: Spec
spec =
  describe "gradient descent" $ do
    it "computes cost" $
      (linearRegressionCost fm rv tt) `shouldSatisfy` (isAround 50.376562500000006) . fst
    it "does linear regression with lin.alg. lib" $
      (let theta = gradientDescent gradDesc fm rv in
        ((fst . linearRegressionCost fm rv) theta, theta))
      `shouldSatisfy` \(cost, theta) ->
          cost `isAround` 0.293648087854516 &&
          theta `isAround` (LA.fromList expectedTheta)
  where
    tt = initModelParams gradDesc fm rv
    fm = addInterceptFeature $ LA.fromRows $ fmap LA.vector xData
    rv = LA.vector yData
    xData = map init trainMatrix
    yData = map last trainMatrix
    gradDesc = defaultGradientDescent {
      numIter = 30000
      , GD.learningRate = 0.005
      , cost = linearRegressionCost
      , lambda = 0
      , addIntercept = False
    }
    expectedTheta =
      [13.781001191549892,
      3.900308511353248e-2,
      -8.360245732382168e-2,
      2.8699265280611834e-2,
      2.7797762929208453e-2,
      0.13913675368346703,
      3.900308511353248e-2,
      0.11340373429004338,
      0.1166082670688832,
      4.446542254295697e-2,
      0.14211000816593214,
      4.446542254295697e-2]
