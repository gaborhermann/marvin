{-# LANGUAGE OverloadedStrings #-}
module Marvin.API.Table.ParseSpec where

import Test.Hspec

import Marvin.API as Marvin
import Marvin.API.Table.Parse

spec :: Spec
spec = do
  readDesc
  parseDesc

readDesc =
  describe "reading" $ do
    it "handles empty" $
      bsToRawTable "" ',' NoHeader `shouldSatisfy` \x -> case x of
          Left (ReadError _) -> True
          _ -> False
    it "handles new lines" $
      bsToRawTable "\n\n\n\n" ',' NoHeader `shouldSatisfy` \x -> case x of
          Left (ReadError _) -> True
          _ -> False
    it "handles new lines with header" $
      bsToRawTable "\n\n\n\n" ',' HasHeader `shouldSatisfy` \x -> case x of
          Left (ReadError _) -> True
          _ -> False
    it "handles row length mismatch" $
      bsToRawTable "1,2,3\n2,3\n1,2,3" ',' NoHeader `shouldSatisfy` \x -> case x of
          Left (RowLengthMismatch _) -> True
          _ -> False

parseDesc =
  describe "parsing" $ do
    it "with scheme" $
      (fromRowsToRaw [["1.2", "1", "0"], ["3", "x", "1"]]
        >>= parseWithScheme [Numeric, Nominal, Binary]) `shouldBe`
      (sequence [
        asDataColumn <$> fromList [1.2 :: Double, 3],
        asDataColumn <$> fromList ["1" :: String, "x"],
        asDataColumn <$> fromList [False, True]] >>= fromColumns)
    it "with scheme fails on wrong types" $
      (fromRowsToRaw [["1.2", "1", "0"], ["3", "x", "1"]]
        >>= parseWithScheme [Natural, Nominal, Binary]) `shouldSatisfy` \x -> case x of
          Left CannotParseColumn -> True
          _ -> False
    it "smart parsing binary" $
      smartParseColumn <$> fromListToRaw ["0","1","1"]
        `shouldBe` asDataColumn <$> fromList [False,True,True]
    it "smart parsing natural" $
      smartParseColumn <$> fromListToRaw ["0","2","1"]
        `shouldBe` asDataColumn <$> (fromList [0,2,1] :: Fallible NaturalColumn)
    it "smart parsing numeric" $
      smartParseColumn <$> fromListToRaw ["0","1.1","1"]
        `shouldBe` asDataColumn <$> (fromList [0,1.1,1] :: Fallible NumericColumn)
    it "smart parsing nominal" $
      smartParseColumn <$> fromListToRaw ["0","1","x"]
        `shouldBe` asDataColumn <$> (fromList ["0","1","x"] :: Fallible NominalColumn)

