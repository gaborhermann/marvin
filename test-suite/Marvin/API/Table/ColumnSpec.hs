module Marvin.API.Table.ColumnSpec where

import Test.Hspec
import Test.QuickCheck

import Marvin.API as Marvin
import Marvin.API.Table.Column

import qualified Data.Vector.Unboxed as UVec
import qualified Data.Vector as Vec

spec :: Spec
spec = do
  genericColumnDesc
  rawColumnDesc
  dataColumnDesc

genericColumnDesc =
  describe "generic Column" $ do
    it "Eq instance gives True on different representation" $
      gcol1 == gcol2 `shouldBe` True
    it "Eq instance gives False on different exposed value" $
      gcol3 == gcol2 `shouldBe` False
    it "Eq instance gives False on different same repr. with different exposed value" $
      gcol1 == gcol4 `shouldBe` False
    it "can be constructed from list" $
      (fromList ["a", "a", "b"] :: Fallible NominalColumn) `shouldBe`
        Right (GC {
          values = UVec.fromList [0,0,1]
          , exposure = \x -> case x of
            0 -> "a"
            1 -> "b"
          , columnNameGen = Nothing
        })
    it "gives correct column size" $
      columnSize numCol `shouldBe` 5
    it "gives correct name" $
      (nameColumn "foo" numCol >>= columnName) `shouldBe` Right "foo"
    it "gives error if name absent" $
      columnName numCol `shouldBe` Left NoColumnName
    it "cannot give name containing _" $
      nameColumn "foo_bar" numCol `shouldSatisfy` \x -> case x of
        Left (InvalidName _) -> True
        _ -> False
    it "cannot be empty" $
      (fromList [] :: Fallible (Column Binary)) `shouldBe` Left EmptyColumn
    it "conversion with list works" $
      property $ \xs -> not (null xs) ==>
        (fmap toList (fromList xs) :: Fallible [String]) `shouldBe` (Right xs)
    it "NaturalColumn cannot be created with negative numbers" $
      (fromList [23, -1, 9] :: Fallible NaturalColumn) `shouldSatisfy` \x -> case x of
         Left (BrokenInvariant _) -> True
         _ -> False

rawColumnDesc =
  describe "RawColumn" $ do
    it "RawColumn cannot be empty" $
      (fromListToRaw [] :: Fallible RawColumn) `shouldBe` Left EmptyColumn
    it "RawColumn conversion with list works" $
      property $ \xs -> not (null xs) ==>
        (fmap rawToList (fromListToRaw xs) :: Fallible [String]) `shouldBe` (Right xs)

dataColumnDesc =
  describe "DataColumn" $ do
    it "can be casted" $
      (asColumn dataCol :: Fallible NumericColumn) `shouldBe` Right numCol
    it "cannot be casted to invalid type" $
      (asColumn dataCol :: Fallible BinaryColumn) `shouldBe` Left CannotCast
    it "gives correct column size" $
      columnSize dataCol `shouldBe` 5
    it "gives correct name" $
      (nameColumn "foo" dataCol >>= columnName) `shouldBe` Right "foo"

gcol1 :: GenericColumn () Int Bool
gcol1 = GC { values = UVec.fromList [0, 1], exposure = even, columnNameGen = Nothing }
gcol2 = GC { values = UVec.fromList [7, 4], exposure = odd, columnNameGen = Just "colName one"}
gcol3 = GC { values = UVec.fromList [2, 2], exposure = odd, columnNameGen = Just "colName two"}
gcol4 = GC { values = UVec.fromList [0, 1], exposure = odd, columnNameGen = Nothing }

numCol :: Column Numeric
numCol = unsafeFromList [1,2,3,4,5]

dataCol :: DataColumn
dataCol = asDataColumn numCol

