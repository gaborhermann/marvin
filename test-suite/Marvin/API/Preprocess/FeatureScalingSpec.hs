module Marvin.API.Preprocess.FeatureScalingSpec (spec) where

import Test.Hspec
import Test.QuickCheck hiding (vector)

import Marvin.API
import Marvin.API.Preprocess.FeatureScaling
import Marvin.Test.TestUtils

spec :: Spec
spec = do
  minMaxScaleDesc
  standarizeDesc

standarizeDesc = describe "standardize" $ do
  it "standardizes column" $
    standardizeColumn original original `shouldSatisfy` isAround expectedStd
  it "ignores standardization on column with constant elements" $
    property $ \col -> standardizeColumn constColumn col `shouldBe` col
  it "makes the elements' avg 0" $ property $
    \v -> notCornerCase v ==> let Right col = fromList v in
      isAround (0 :: Double) $ avg $ toList $ standardizeColumn col col
  it "makes the elements' standard deviation 1" $ property $
    \v -> notCornerCase v ==> let Right col = fromList v in
      isAround (1 :: Double) $ stdDeviation $ toList $ standardizeColumn col col

minMaxScaleDesc = describe "minMaxScale" $ do
  it "scales column by min-max" $
    minMaxScaleColumn original original `shouldSatisfy` isAround expectedMinMax
  it "ignores scaling on column with constant elements" $
    property $ \col -> minMaxScaleColumn constColumn col `shouldBe` col
  it "makes the minimum 0" $ property $
    \v -> notCornerCase v ==> let Right col = fromList v in
      isAround (0 :: Double) $ minimum $ toList $ minMaxScaleColumn col col
  it "makes the maximum 1" $ property $
    \v -> notCornerCase v ==> let Right col = fromList v in
      isAround (1 :: Double) $ maximum $ toList $ minMaxScaleColumn col col

avg v             = sum v / fromIntegral (length v)
stdDeviation v    = sqrt $ (sum . fmap (\x -> (x - avg v)^2)) v / fromIntegral (length v)

notCornerCase :: [Double] -> Bool
notCornerCase v =
  length v >= 1 &&
  not (isAround 0.0 (maximum v)) &&
  stdDeviation v /= 0

Right constColumn = fromList
  [8,8,8,8,8]
Right original = fromList
  [19,9,4,10,23]
Right expectedMinMax = fromList
  [0.7894736842105,0.2631578947368,0.0,0.3157894736842,1.0]
Right expectedStd = fromList
  [0.862439361864104,-0.574959574576069,-1.29365904279616,-0.431219680932052,1.43739893644017]
