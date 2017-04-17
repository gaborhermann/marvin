module Marvin.API.Preprocess.OneHotEncodeSpec where

import Test.Hspec

import Data.Either (isRight)

import Marvin.API

spec :: Spec
spec = describe "oneHotEncode" $ do
  it "encodes nominal table to binary" $
    oneHotEncode nominalTab nominalTab `shouldBe` Right binaryTab
  it "encodes different nominal table to binary" $
    oneHotEncode nominalTab otherNominalTab `shouldBe` Right otherBinaryTab
  it "no column name conflict" $
    oneHotEncode colNameConflictTab colNameConflictTab `shouldSatisfy` \x -> case x of
        Right tab -> isRight (fromColumns (columns tab))
        _ -> False
  it "throws error for wrong dimensions" $
    oneHotEncode nominalTab wrongDimensionsTab `shouldSatisfy` \x -> case x of
        Left (RowLengthMismatch _) -> True
        _ -> False

Right nominalTab = fromRows
   [["a", "x"]
  , ["b", "x"]
  , ["a", "y"]
  , ["a", "x"]
  , ["c", "y"]]

Right binaryTab = fromRows
   [[x,o,o, x,o]
  , [o,x,o, x,o]
  , [x,o,o, o,x]
  , [x,o,o, x,o]
  , [o,o,x, o,x]]

Right otherNominalTab = fromRows
   [["d", "a"]
  , ["d", "x"]
  , ["c", "d"]]

Right otherBinaryTab = fromRows
   [[o,o,o, o,o]
  , [o,o,o, x,o]
  , [o,o,x, o,o]]

Right colNameConflictTab = fromRows
   [["a", "x"]
  , ["x", "x"]
  , ["a", "y"]
  , ["x", "x"]
  , ["c", "y"]]

Right wrongDimensionsTab = fromRows
   [["a", "x", "p"]
  , ["a", "y", "q"]
  , ["c", "y", "r"]]

x = True
o = False
