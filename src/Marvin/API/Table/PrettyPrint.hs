{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Module      : Marvin.API.Table.PrettyPrint
Description : Pretty printing tables and columns.
-}
module Marvin.API.Table.PrettyPrint where

import Control.Arrow

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vec
import qualified Data.Vector.Unboxed as UVec
import Data.List (find, intersperse)
import Data.Maybe (isNothing)
import Data.Either (isRight)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)

import Text.PrettyPrint.Boxes hiding (columns)
import qualified Text.PrettyPrint.Boxes as Boxes
import Text.Printf

import Marvin.API.Table.DataType
import Marvin.API.Fallible

import Marvin.API.Table.Internal
import Data.Typeable

import Control.Monad.Reader

-- * Pretty printable types

instance PrettyPrintable String where
  prettyPrint = return

instance PrettyPrintable Double where
  prettyPrint x = do
    numOfDecs <- asks numberOfDecimals
    return $ printf ("%." ++ show numOfDecs ++ "f") x

instance PrettyPrintable Bool where
  prettyPrint b = return $ show $ if b then 1 else 0

instance PrettyPrintable Text where
  prettyPrint = return . show

instance PrettyPrintable ByteString where
  prettyPrint = return . unpack

instance PrettyPrintable Int where
  prettyPrint = return . show

-- * Default configurations for showing

defaultPrettyPrintParams = PrettyPrintParams {
  numberOfDecimals = 3,
  maxNumberOfLinesToShow = 60,
  maxNumberOfColumnsInWindow = 5
}

withDefaultPrettyPrint :: Reader PrettyPrintParams a -> a
withDefaultPrettyPrint reader = runReader reader defaultPrettyPrintParams

defaultPrettyPrint :: PrettyPrintable a => a -> String
defaultPrettyPrint x = (runReader (prettyPrint x)) defaultPrettyPrintParams

defaultShowColumn :: (ColumnDataType a, PrettyPrintable (ColumnExposed a)) =>
  Column a -> String
defaultShowColumn col = runReader paramShown defaultPrettyPrintParams
  where
    paramShown = do
      bs <- prettyPrintColumnWithName col
      return $ "\n" ++ render (vcat Boxes.right (Vec.toList bs))

defaultShowTable :: (ColumnDataType a, PrettyPrintable (ColumnExposed a)) =>
  Table a -> String
defaultShowTable t = runReader (prettyPrintTable t) defaultPrettyPrintParams

defaultShowDataTable t = runReader (prettyPrintDataTable t) defaultPrettyPrintParams


-- * 'Show' instances for tables and columns --

-- ** Columns

instance Show DataColumn where
  show (DC c) = defaultShowColumn c

instance (ColumnDataType a, PrettyPrintable e,
  ColumnRepr a ~ r, ColumnExposed a ~ e) => Show (GenericColumn a r e) where
  show = defaultShowColumn

-- ** Tables

instance (ColumnDataType a, PrettyPrintable e,
  ColumnRepr a ~ r, ColumnExposed a ~ e) => Show (GenericTable a r e) where
  show = defaultShowTable

instance Show DataTable where
  show = defaultShowDataTable

instance Show RawTable where
  show t = runReader (ppTable prettyPrintColumnRaw t) defaultPrettyPrintParams

-- * Column pretty printing

prettyPrintColumnWithName :: (ColumnDataType a, PrettyPrintable (ColumnExposed a)) =>
  Column a -> Reader PrettyPrintParams (Vector Box)
prettyPrintColumnWithName col = do
  values <- prettyPrintColumn col
  let name = nameBox $ columnName col
  return $ Vec.cons name values

prettyPrintColumnRaw :: RawColumn -> Reader PrettyPrintParams (Vector Box)
prettyPrintColumnRaw col = do
  let colExposed = valuesRaw col
  prettyPrintVector colExposed

prettyPrintColumn :: (ColumnDataType a, PrettyPrintable (ColumnExposed a)) =>
  Column a -> Reader PrettyPrintParams (Vector Box)
prettyPrintColumn col = do
  let colExposed = Vec.map (exposure col) $ UVec.convert $ values col
  prettyPrintVector colExposed

ppDataColumn :: DataColumn -> Reader PrettyPrintParams (Vector Box)
ppDataColumn (DC col) = prettyPrintColumn col

-- | Pretty prints a vector by limiting the lines to show.
-- Shows the first n and last n values in a large column.
prettyPrintVector :: (PrettyPrintable a) =>
  Vector a -> Reader PrettyPrintParams (Vector Box)
prettyPrintVector vec = do
  maxLines <- asks maxNumberOfLinesToShow
  let dotBox = Vec.singleton $ text "..."

  let (startCol, endCol) = limitedLines maxLines vec
  start <- ppVec startCol
  end <- ppVec endCol
  let splitWithDots = start Vec.++ dotBox Vec.++ end

  if Vec.length vec <= maxLines + 1
    then ppVec vec
    else return $ splitWithDots

ppVec :: (PrettyPrintable a) => Vector a -> Reader PrettyPrintParams (Vector Box)
ppVec v = do
  vecStrings <- traverse prettyPrint v
  return $ Vec.map text vecStrings

-- | Splits a large column to a limited number of head elements and tail elements.
limitedLinesCol :: (ColumnDataType a) => Int -> Column a -> (Column a, Column a)
limitedLinesCol maxLines col = (col { values = UVec.convert v1 }, col { values = UVec.convert v2 })
  where
    (v1, v2) = limitedLines maxLines $ UVec.convert $ values col

-- | Splits a large 'Vector' to a limited number of head elements and tail elements.
limitedLines :: Int -> Vector a -> (Vector a, Vector a)
limitedLines maxLines xs = (Vec.take n xs, Vec.drop (Vec.length xs - m) xs)
  where
    m = maxLines `div` 2
    n = m + maxLines `mod` 2

-- | Creates a 'Box' from a name.
nameBox :: Fallible String -> Box
nameBox (Right name) = text name
nameBox (Left _) = emptyBox 1 1


-- * Table pretty printing

-- | Pretty prints a table with a given column pretty printer function.
ppTable :: (TableClass t) => (ColumnType t -> Reader PrettyPrintParams (Vector Box)) ->
  t -> Reader PrettyPrintParams String
ppTable ppColumn t = if n == 0 then return ("Empty table " ++ rowsAndCols) else do
    colValuesVec <- traverse ppColumn cols
    let colValues = fmap Vec.toList colValuesVec
    -- data columns aligned right
    let cols = fmap (vcat Boxes.right) colValues
    -- attached horizontal indices, names, and columns
    let tableBox = zipWith3 (\idx name vals -> vcat Boxes.right [idx,name,vals]) hIdx colNames cols
    -- vertical indices
    vIdxVec <- indexes 0 (m-1)
    -- vertical indices aligned left
    let vIdx = vcat Boxes.left $ (nameAboveVIdx : Vec.toList vIdxVec)
    -- partitioning to blocks
    blockSize <- asks maxNumberOfColumnsInWindow
    let blocks = slicesOfSize blockSize $ tableBox
    -- insert empty space between columns
    let blockToBox block = hcat Boxes.bottom (vIdx : emptyBox 1 1 : intersperse (emptyBox 1 2) block)
    let box = vcat Boxes.left $ intersperse (emptyBox 1 1) $ fmap blockToBox blocks
    return $ "\n" ++ render box  ++ "\n" ++ rowsAndCols
    where
      -- number of columns
      n = numberOfColumns t
      -- horizontal indexes
      hIdx = (map (text . show) [0..(n-1)])
      -- indicating the size
      rowsAndCols = "["
        ++ show (numberOfRows t) ++ " rows x "
        ++ show (numberOfColumns t) ++ " columns]"
      cols = Vec.toList $ columnsVec t
      m = numberOfRows t
      -- check if there is no column name
      noColNames = isNothing $ find (isRight . columnName) $ cols
      colNames = if noColNames
        then (fmap (const nullBox) cols)
        else (fmap ppColName cols)
      nameAboveVIdx = if noColNames
        then nullBox
        else emptyBox 1 1

prettyPrintTable :: (ColumnDataType a, PrettyPrintable (ColumnExposed a)) =>
  Table a -> Reader PrettyPrintParams String
prettyPrintTable t = ppTable prettyPrintColumn t

prettyPrintDataTable :: DataTable -> Reader PrettyPrintParams String
prettyPrintDataTable = ppTable ppDataColumn

-- | Creates a 'Box' from a column name.
ppColName :: (ColumnClass c) => c -> Box
ppColName = columnName >>> nameBox

-- | Creates a 'Vector Box' from a given range of integers.
indexes :: Int -> Int -> Reader PrettyPrintParams (Vector Box)
indexes from to = prettyPrintVector $ Vec.fromList [from..to]

-- | Slices up a list to lists of a given size.
slicesOfSize :: Int -> [a] -> [[a]]
slicesOfSize k = iterate (drop k)
  >>> takeWhile (not . null)
  >>> map (take k)

