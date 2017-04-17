{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-|
Module      : Marvin.API.Table.Internal
Description : Statically and dynamically typed tables that represent machine learning data.
-}
module Marvin.API.Table.Internal (
  module Marvin.API.Table.Internal
  , module Column
) where

import Data.Vector hiding (map, length, head, (++), zipWith)
import qualified Data.Vector as Vec
import qualified Data.Vector.Unboxed as UVec
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.List as List
import Data.Either
import Data.Maybe

import Control.Monad.Except

import Control.Arrow

import Marvin.API.Fallible
import Marvin.API.Table.DataType
import Marvin.API.Table.Column as Column

-- * TableClass

class (ColumnClass (ColumnType t)) => TableClass t where
  -- | Injective type family assigning a column type to the table type.
  type ColumnType t = c | c -> t
  -- | Retrieves the columns in a 'Vector'
  columnsVec :: t -> Vector (ColumnType t)
  -- | Creating from a 'Vector' without checks.
  unsafeFromCols :: Vector (ColumnType t) -> t
  -- | Retrieves the 'Map' with the indices of column names.
  nameIndex :: t -> Map String Int

-- * Concrete table types

-- ** RawTable

-- | Type representing a raw, unparsed table.
newtype RawTable = RawTable (Map ColumnName Int, Vector RawColumn)

instance TableClass RawTable where
  type ColumnType RawTable = RawColumn
  columnsVec (RawTable (_, cols)) = cols
  unsafeFromCols cols = RawTable (mkNameIndex cols, cols)
  nameIndex (RawTable (nameIdx, _)) = nameIdx

instance Eq RawTable where
  tab1 == tab2 = columnsVec tab1 == columnsVec tab2


-- ** GenericTable

-- | Type for homogenous, statically typed, parsed table.
newtype GenericTable a r e = GenericTable (Map ColumnName Int, Vector (GenericColumn a r e))

instance (ColumnDataType a, UVec.Unbox r, Eq e) => TableClass (GenericTable a r e) where
  type ColumnType (GenericTable a r e) = GenericColumn a r e
  columnsVec (GenericTable (_, cols)) = cols
  unsafeFromCols cols = GenericTable (mkNameIndex cols, cols)
  nameIndex (GenericTable (nameIdx, _)) = nameIdx

instance (ColumnDataType a, UVec.Unbox r, Eq e) => Eq (GenericTable a r e) where
  tab1 == tab2 = columnsVec tab1 == columnsVec tab2

-- *** Type aliases for GenericTables
type Table a = GenericTable a (ColumnRepr a) (ColumnExposed a)
type NumericTable = GenericTable Numeric Double Double
type NominalTable = GenericTable Nominal Int String
type BinaryTable = GenericTable Binary Bool Bool
type NaturalTable = GenericTable Natural Int Int


-- ** DataTable

-- | Dynamically typed table.
-- The table type could be numeric, nominal, binary, or natural.
newtype DataTable = DT (Map ColumnName Int, Vector DataColumn)

instance TableClass DataTable where
  type ColumnType DataTable = DataColumn
  columnsVec (DT (_, cols)) = cols
  unsafeFromCols cols = DT (mkNameIndex cols, cols)
  nameIndex (DT (nameIdx, _)) = nameIdx

instance Eq DataTable where
  tab1 == tab2 = columnsVec tab1 == columnsVec tab2

-- | Types of the columns in a 'DataTable'.
columnTypes :: DataTable -> [DataType]
columnTypes = columns >>> fmap columnType

-- | Creates dynamically typed table from statically typed.
asDataTable :: (ColumnDataType a, PrettyPrintable (ColumnExposed a)) => Table a -> DataTable
asDataTable (GenericTable (nameIdx, cols)) = DT (nameIdx, fmap asDataColumn cols)

-- | Tries to creates statically typed table from dynamically typed.
asTable :: ColumnDataType a => DataTable -> Fallible (Table a)
asTable dt = do
    -- checking and creating 'NumericColumn's
    numCols <- traverse asColumn $ columnsVec dt
    return $ unsafeFromCols numCols

-- | Retrieves all the columns with a given type from a dynamically typed table.
suitableColumns :: ColumnDataType a => DataTable -> Table a
suitableColumns dataTab = unsafeFromCols $ vecRights $ fmap asColumn cols
  where
    vecRights = Vec.toList >>> rights >>> Vec.fromList
    cols = columnsVec dataTab

-- | Retrieves all the numeric columns from a dynamically typed table.
numericColumns :: DataTable -> NumericTable
numericColumns = suitableColumns

-- | Retrieves all the nominal columns from a dynamically typed table.
nominalColumns :: DataTable -> NominalTable
nominalColumns = suitableColumns

-- | Retrieves all the binary columns from a dynamically typed table.
binaryColumns :: DataTable -> BinaryTable
binaryColumns = suitableColumns

-- | Retrieves all the natural columns from a dynamically typed table.
naturalColumns :: DataTable -> NaturalTable
naturalColumns = suitableColumns

-- | Retrieves a column with a given type from a dynamically typed table.
getColumnOfType :: (ColumnDataType a) => a -> ColumnId -> DataTable -> Fallible (Column a)
getColumnOfType _ colId t = do
    col <- getColumn colId t
    asColumn col

-- * Table constructors

-- | Constructs empty table
emptyTable :: TableClass t => t
emptyTable = unsafeFromCols Vec.empty

-- | Creates table from a list of columns.
fromColumns :: TableClass t => [ColumnType t] -> Fallible t
fromColumns = Vec.fromList >>> fromColumnsVec

-- | Creates table from a list of columns with names.
fromColumnsWithNames :: TableClass t => [(String, ColumnType t)] -> Fallible t
fromColumnsWithNames cols = do
  namedCols <- traverse (\(name, col) -> nameColumn name col) cols
  fromColumns namedCols

-- | Creates statically typed table from a nested list of values in rows.
fromRows :: (ColumnDataType a) => [[ColumnExposed a]] -> Fallible (Table a)
fromRows = map Vec.fromList >>> Vec.fromList >>> fromRowsVec

-- | Creates 'RawTable' from a nested list of values in rows.
fromRowsToRaw :: [[String]] -> Fallible RawTable
fromRowsToRaw rows = do
  cols <- rowsToCols $ Vec.fromList $ map Vec.fromList rows
  let cols' = fmap (fromVectorToRaw . Vec.map stringToBS) cols
  fromColumns $ Vec.toList cols'

fromColumnsVec :: TableClass t => Vector (ColumnType t) -> Fallible t
fromColumnsVec cols = do
  validCols <- traverse (columnWithSize ("expected size " ++ show n) n) cols
  return $ unsafeFromCols validCols
  where n = columnSize $ Vec.head cols

fromRowsVec :: (ColumnDataType a) => Vector (Vector (ColumnExposed a)) -> Fallible (Table a)
fromRowsVec rows = do
  colVectors <- rowsToCols rows
  cols <- traverse (Column.fromList . Vec.toList) colVectors
  return $ unsafeFromCols cols


-- * Table operations

-- | Retrieves the column from a table.
columns :: TableClass t => t -> [ColumnType t]
columns = Vec.toList . columnsVec

-- | Retrieves the number of columns in a table.
numberOfColumns :: TableClass t => t -> Int
numberOfColumns = columnsVec >>> Vec.length

-- | Retrieves the number of rows in a table.
numberOfRows :: TableClass t => t -> Int
numberOfRows t = if Vec.null cols then 0 else columnSize $ Vec.head cols
  where cols = columnsVec t

-- | Splits the table to a target variable and the remaining columns.
selectTargetVariable :: TableClass t => ColumnId -> t -> Fallible (t, ColumnType t)
selectTargetVariable colId table = do
  features <- removeColumn colId table
  targetCol <- getColumn colId table
  return (features, targetCol)

-- | Removes a column from a table.
removeColumn :: TableClass t => ColumnId -> t -> Fallible t
removeColumn colId table = do
    i <- columnIdToIndex colId table
    let cols' = removeAt i cols
    return $ unsafeFromCols cols'
  where
    removeAt i v = unsafeTake i v Vec.++ unsafeDrop (i+1) v
    cols = columnsVec table

-- | Takes the union of columns from a list of tables.
featureUnion :: TableClass t => [t] -> Fallible t
featureUnion [] = return emptyTable
featureUnion tabs@(t:ts) = do
  let errorMsg = "While taking the union of tables."
  -- column size check
  traverse (ensureTableSizeMatch errorMsg t) ts
  -- naming check
  ensureNoNameConflict errorMsg tabs
  return $ unsafeFromCols $
    Vec.concat $ List.map columnsVec tabs

-- | Appends a column to a table.
appendColumn :: TableClass t => t -> ColumnType t -> Fallible (t)
appendColumn table col = do
  ensureTableColSizeMatch
    ("While appending column. Column has " ++ show (columnSize col) ++
      " elements, table has " ++ show (numberOfRows table) ++ " rows.")
    table col
  case columnName col of
    Right name -> if Map.member name (nameIndex table)
        then throwError $ ColumnNameConflict $ "While appending column. " ++
          "Table already has a column named \"" ++ name ++ "\"."
        else return ()
    _ -> return ()
  return $ unsafeFromCols $ Vec.snoc (columnsVec table) col

-- | Retrieves a column from a table.
getColumn :: TableClass t => ColumnId -> t -> Fallible (ColumnType t)
getColumn colId table = do
  idx <- columnIdToIndex colId table
  return $ cols Vec.! idx
    where
      cols = columnsVec table

-- | Retrieves a column names from a table.
columnNames :: TableClass t => t -> [Fallible ColumnName]
columnNames = map columnName . columns


-- * Checks

-- | Checks if column has the given size, throws error the a message if not.
columnWithSize :: TableClass t => String -> Int -> ColumnType t -> Fallible (ColumnType t)
columnWithSize msg n col = if columnSize col == n
  then Right col
  else Left $ ColumnSizeMismatch msg

-- | Checks if table is empty.
ensureNonEmptyTable :: TableClass t => String -> t -> Fallible ()
ensureNonEmptyTable msg t = if numberOfColumns t == 0
  then throwError $ EmptyTable msg
  else return ()

-- | Checks if the number of rows of a table is equal to the size of a column.
ensureTableColSizeMatch :: TableClass t => String -> t -> ColumnType t -> Fallible ()
ensureTableColSizeMatch msg t c =
  if numberOfRows t == columnSize c
  then return ()
  else throwError $ ColumnSizeMismatch msg

-- | Checks if two tables have the same number of rows.
ensureTableSizeMatch :: (TableClass t, TableClass t') => String -> t -> t' -> Fallible ()
ensureTableSizeMatch msg t1 t2 =
  if numberOfRows t1 == numberOfRows t2 || numberOfRows t1 == 0 || numberOfRows t2 == 0
  then return ()
  else throwError $ ColumnSizeMismatch msg

-- | Checks if there's a conflict between names in a list of columns.
ensureNoNameConflictColumns :: ColumnClass c => String -> [c] -> Fallible ()
ensureNoNameConflictColumns msg cols = do
  let allNames = rights $ List.map columnName cols
  if Set.size (Set.fromList allNames) /= List.length allNames
    then throwError $ ColumnNameConflict msg
    else return ()

-- | Checks if there's a conflict between names in columns of tables.
ensureNoNameConflict :: TableClass t => String -> [t] -> Fallible ()
ensureNoNameConflict msg tabs = do
  let allNames = List.concatMap (Map.keys . nameIndex) tabs
  if Set.size (Set.fromList allNames) /= List.length allNames
    then throwError $ ColumnNameConflict msg
    else return ()

-- | Checks if two tables have the same number of columns.
withMatchingRowLength :: TableClass t => String -> t -> t -> Fallible ()
withMatchingRowLength msg t1 t2 =
  if numberOfColumns t1 == numberOfColumns t2
  then return ()
  else throwError (RowLengthMismatch msg)

-- * Transforming

-- | Transforms every column by a trainable transformation. Kind of a flat map operation.
transformEveryColumn :: (TableClass t, TableClass t') =>
  String -- ^ Error message
  -> (ColumnType t -> (ColumnType t -> [ColumnType t'])) -- ^ Column transformer
  -> t -> (t -> Fallible t') -- ^ Resulting table transformers
transformEveryColumn msg columnTransformer table table' = do
  withMatchingRowLength msg table table'
  let cols = List.concat $ zipWith ($) columnTransformers $ (columns table')
  ensureNoNameConflictColumns msg cols
  return $ unsafeFromCols $ Vec.fromList $ cols
  where
    columnTransformers = List.map columnTransformer $ columns table


-- * Column identifiers

-- | Type for column identifiers
data ColumnId = Named ColumnName | Indexed Int | LastColumn

-- ** Constructors

-- | Name identifier.
byName :: ColumnName -> ColumnId
byName = Named

-- | Index identifier.
byIndex :: Int -> ColumnId
byIndex = Indexed

-- | Identifier for the last column.
lastColumn = LastColumn

-- | Creates the index of a column in a table from a 'ColumnId' if such column exists in the table.
columnIdToIndex :: TableClass t => ColumnId -> t -> Fallible Int
columnIdToIndex colId table = case colId of
  Indexed i -> if i >= numberOfColumns table
    then throwError $ NoSuchColumn $ "at index " ++ show i
    else return i
  LastColumn -> if numberOfColumns table == 0
    then throwError $ NoSuchColumn $ "last column (the table is empty)"
    else return $ numberOfColumns table - 1
  Named name -> maybe
    (throwError (NoSuchColumn ("with name \"" ++ name ++ "\"")))
    return
    $ Map.lookup name (nameIndex table)


-- * Conversions

binaryToNumericColumn :: BinaryColumn -> NumericColumn
binaryToNumericColumn = mapColumnAtoB (\x -> if x then 1 else 0)

naturalToNumericColumn :: NaturalColumn -> NumericColumn
naturalToNumericColumn = mapColumnAtoB fromIntegral

nominalToBinaryColumn :: NominalColumn -> Fallible BinaryColumn
nominalToBinaryColumn nomCol = if length (distinctValues nomCol) /= 2
  then throwError $ CannotCast
  else return $ copyName (columnNameGen nomCol) $ unsafeFromVector $ UVec.convert $
    UVec.map (\x -> if x == 1 then True else False) $ values nomCol

binaryToNumeric :: BinaryTable -> NumericTable
binaryToNumeric = columnsVec >>> Vec.map binaryToNumericColumn >>> unsafeFromCols

naturalToNumeric :: NaturalTable -> NumericTable
naturalToNumeric = columnsVec >>> Vec.map naturalToNumericColumn >>> unsafeFromCols

nominalToBinary :: NominalTable -> Fallible BinaryTable
nominalToBinary = columnsVec >>> traverse nominalToBinaryColumn >>> fmap unsafeFromCols


-- * Helpers

-- | Creates a name index from a 'Vector' of columns.
mkNameIndex :: (ColumnClass c) => Vector c -> Map ColumnName Int
mkNameIndex cols = Map.fromList $ catMaybes idxColNames
  where
    idxColNames = zipWith (\i maybeName -> fmap (\name -> (name, i)) maybeName) [0..] colNames
    colNames = fmap (fallibleToMaybe . columnName) $ Vec.toList cols

-- | Transposes nested 'Vector' with size check.
rowsToCols :: Vector (Vector a) -> Fallible (Vector (Vector a))
rowsToCols rs = maybe (Left (RowLengthMismatch "When attempting to create table from rows."))
  Right $ sequenceA $ Vec.map sequenceA maybeCells
  where
    len = Vec.length $ Vec.head rs
    colIds = Vec.fromList [0..(len-1)]
    maybeCells = Vec.map (\i -> Vec.map (!? i) rs) colIds

-- | Transposes nested 'Vector' without size check.
transposeVectorVector :: Vector (Vector a) -> Vector (Vector a)
transposeVectorVector cs = rows
  where
    len = Vec.length $ Vec.head cs
    rowIds = Vec.fromList [0..(len-1)]
    rows = Vec.map (\i -> Vec.map (! i) cs) rowIds

-- | Transposes nested unboxed 'Vector' without size check.
transposeVectorVectorU :: (UVec.Unbox a) => Vector (UVec.Vector a) -> Vector (UVec.Vector a)
transposeVectorVectorU cs = Vec.fromList $ fmap UVec.fromList xssT
  where
    xss = Vec.toList $ Vec.map UVec.toList cs
    xssT = List.transpose xss