module Marvin.API.Table.InternalSpec (spec) where

import Test.Hspec
import Test.QuickCheck hiding (vector)

import Marvin.API.Table.Internal
import Marvin.API.Table.Column
import Marvin.API.Table.DataType
import Marvin.API.Fallible
import Marvin.Test.TestUtils

import qualified Data.Vector as Vec (map,fromList,empty)

spec :: Spec
spec = do
  tableDesc
  tableIntegrationTest
  dataTableDesc

tableIntegrationTest = describe "Table operations" $ do
  it "can be combined" $
    (do
      cols <- sequence $
        [
          fromList [1,2] >>= nameColumn "a"
          , fromList [3,4] >>= nameColumn "b"
          , fromList [5,6]
          , fromList [7,8] >>= nameColumn "d"
        ]
      tab0 <- fromColumns cols :: Fallible NumericTable
      col <- fromList [9,10] >>= nameColumn "z"
      tab1 <- appendColumn tab0 col
      tab2 <- removeColumn (byIndex 1) tab1
      selectTargetVariable lastColumn tab2)
        `shouldBe` (do
                cols <- sequence $ [
                    fromList [1,2] >>= nameColumn "a"
                    , fromList [5,6]
                    , fromList [7,8] >>= nameColumn "d"
                    ]
                tab <- fromColumns cols
                target <- fromList [9,10] >>= nameColumn "z"
                return (tab, target)
            )

tableDesc = describe "Table" $ do
  it "can be created from rows or from columns" $
    (fromRows [[1,2],[3,4],[5,6]] :: Fallible NumericTable) `shouldBe`
      (traverse fromList [[1,3,5],[2,4,6]] >>= fromColumns)
  it "cannot be created from rows with different length" $
    (fromRows [[1,2],[3],[5,6]] :: Fallible NaturalTable) `shouldSatisfy` \res -> case res of
      Left (RowLengthMismatch _) -> True
      _ -> False
  it "retrieves column by index" $
    (do
      nt <- fromRows [[1,2,3],[10,20,30]]
      getColumn (byIndex 1) nt) `shouldBe` (fromList [2,20] :: Fallible NumericColumn)
  it "removes column by index" $
    (do
       nt <- fromRows [[1,2,3],[10,20,30]]
       removeColumn (byIndex 1) nt) `shouldBe` (fromRows [[1,3],[10,30]] :: Fallible NumericTable)
  it "retrieves column by name" $
    getColumn (byName "b") named `shouldBe` Right colB
  it "selects target variable by name" $
    selectTargetVariable (byName "a") named `shouldBe` do
      tab <- fromColumns [colB, colC]
      return (tab, colA)
  it "can append column" $
    appendColumn named sameSizeColZ `shouldBe` Right (fromListNamed
      ["a", "b", "c", "z"]
      [[1,2],[10,20],[100,200],[1,2]])
  it "cannot append column with name conflict" $
    appendColumn tableForNameConflict sameSizeColZ `shouldSatisfy` \x -> case x of
      Left (ColumnNameConflict _) -> True
      _ -> False
  it "cannot append column with different size" $
    appendColumn named differentSizeCol `shouldSatisfy` \x -> case x of
      Left (ColumnSizeMismatch _) -> True
      _ -> False
  it "can union tables" $
    featureUnion [named, tableForUnion] `shouldBe` Right featureUnionTable
  it "cannot union tables with name conflict" $
    featureUnion [named, tableForNameConflict] `shouldSatisfy` \x -> case x of
      Left (ColumnNameConflict _) -> True
      _ -> False
  it "cannot union tables with different size" $
    featureUnion [named, differentSizeTab] `shouldSatisfy` \x -> case x of
      Left (ColumnSizeMismatch _) -> True
      _ -> False

dataTableDesc = describe "DataTable" $ do
  it "types can be queried" $
    columnTypes <$> tab `shouldBe` return [Numeric, Binary, Natural, Binary]
  it "numeric columns can be extracted" $
    numericColumns <$> tab `shouldBe` numTab
  it "natural columns can be extracted" $
    naturalColumns <$> tab `shouldBe` natTab
  it "binary columns can be extracted" $
    binaryColumns <$> tab `shouldBe` binTab
  it "can be casted" $
    (tab >>= removeColumn (byIndex 2) >>= removeColumn (byName "a") >>= asTable) `shouldBe` binTab
  where
    tab = sequence [
          asDataColumn <$> (fromList [1,2] >>= nameColumn "a" :: Fallible NumericColumn)
          , asDataColumn <$> (fromList [True,True] >>= nameColumn "b" :: Fallible BinaryColumn)
          , asDataColumn <$> (fromList [7,8] >>= nameColumn "d" :: Fallible NaturalColumn)
          , asDataColumn <$> (fromList [False,True] :: Fallible BinaryColumn)
        ] >>= fromColumns
    numTab = fromList [1,2] >>= nameColumn "a" >>= (\c -> fromColumns [c])
    natTab = fromList [7,8] >>= nameColumn "d" >>= (\c -> fromColumns [c])
    binTab = sequence [fromList [True,True] >>= nameColumn "b", fromList [False,True]]
      >>= fromColumns

-- helpers
fromListNamed :: [String] -> [[Double]] -> NumericTable
fromListNamed names colsList = tab
  where
    Right tab = do
      cols <- traverse fromList colsList
      fromColumnsWithNames (Prelude.zip names cols)

named = fromListNamed
  ["a", "b", "c"]
  [[1,2],[10,20],[100,200]]

tableForUnion = fromListNamed
  ["p", "q"]
  [[1,2],[10,20]]

featureUnionTable = fromListNamed
  ["a", "b", "c", "p", "q"]
  [[1,2],[10,20],[100,200],[1,2],[10,20]]

differentSizeTab = fromListNamed
  ["x", "y", "z"]
  [[1,2,3],[10,20,30],[100,200,300]]

tableForNameConflict = fromListNamed
  ["a", "y", "z"]
  [[1,2],[10,20],[100,200]]

[colA, colB, colC] = columns named

differentSizeCol :: NumericColumn
Right differentSizeCol = fromList [1,2,3]

sameSizeColZ :: NumericColumn
Right sameSizeColZ = fromList [1,2] >>= nameColumn "z"
