module Marvin.API.FallibleSpec where

import Test.Hspec
import Test.QuickCheck

import Marvin.Test.TestUtils

import Marvin.API.Fallible

spec :: Spec
spec =
  describe "Fallible" $ do
    it "ensuring property throws error" $
      property $ \error x ->
        ensureProperty (const False) error (x :: Int) `shouldBe` (Left error)
    it "ensuring property works when property satisfied" $
      property $ \error x ->
        ensureProperty (const True) error (x :: String) `shouldBe` (Right ())
    it "orElse works 1 " $
      property $ \error x ->
        (Right x :: Fallible Int) `orElse` (Left error) `shouldBe` (Right x)
    it "orElse works 2" $
      property $ \error x ->
        (Left error) `orElse` (x :: Fallible Int) `shouldBe` x
