{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-|
Module      : Marvin.API.Table.Column
Description : Statically and dynamically typed columns that represent machine learning attributes.
-}
module Marvin.API.Table.Column where

import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Data.Vector.Unboxed as UVec

import qualified Data.Set as Set

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.ByteString.Internal (c2w, w2c)

import Data.Typeable
import Control.Arrow
import Control.Monad.Reader
import Control.Monad.Except

import Text.PrettyPrint.Boxes (Box)

import Marvin.API.Table.DataType
import Marvin.API.Fallible

type ColumnName = String

-- * ColumnClass

-- | Class for general columns.
class Eq c => ColumnClass c where
  -- | Number of elements in column.
  columnSize :: c -> Int
  -- | Name of column.
  columnName :: c -> Fallible ColumnName
  -- | Names a column without checking name validity.
  nameColumn' :: ColumnName -> c -> c
  -- | Splits a column by 'Bool' flags.
  -- Useful for random splitting tables to training and test data.
  splitByFlags :: [Bool] -> c -> Fallible (c, c)

-- * Concrete column types

-- ** RawColumn

-- | Type for column before parsing. Contains Strings.
newtype RawColumn = RawColumn (Maybe ColumnName, Vector ByteString)

instance ColumnClass RawColumn where
  columnSize (RawColumn (_, vals)) = Vec.length vals
  columnName (RawColumn (name, _)) = maybe (Left NoColumnName) Right name
  nameColumn' name (RawColumn (_, vals)) = RawColumn (Just name, vals)
  splitByFlags = splitByFlagsRaw

instance Show RawColumn where
  show (RawColumn (_, vals)) = show $ Vec.cons (Vec.last vals) (Vec.take 10 vals)

instance Eq RawColumn where
  a == b = valuesRaw a == valuesRaw b

-- | Creating 'RawColumn' from list.
fromListToRaw :: [String] -> Fallible RawColumn
fromListToRaw xs = do
  -- checking whether the column is empty
  checkNonEmptyColumn xs
  -- creating a ByteStrings from Strings, and Vector from the list.
  return $ fromVectorToRaw $ Vec.fromList $ fmap stringToBS xs

-- | Creating 'RawColumn' from list.
rawToList :: RawColumn -> [String]
rawToList = valuesRaw >>> Vec.toList >>> fmap bsToString

-- *** Helper methods

bsToString :: ByteString -> String
bsToString = BS.unpack >>> fmap w2c

stringToBS :: String -> ByteString
stringToBS  = fmap c2w >>> BS.pack

valuesRaw :: RawColumn -> Vector ByteString
valuesRaw (RawColumn (_, vals)) = vals

setValuesRaw :: Vector ByteString -> RawColumn -> RawColumn
setValuesRaw vals (RawColumn (name, _)) = RawColumn (name, vals)

fromVectorToRaw :: Vector ByteString -> RawColumn
fromVectorToRaw vals = RawColumn (Nothing, vals)

columnNameRaw :: RawColumn -> Maybe ColumnName
columnNameRaw (RawColumn (name, _)) = name


-- ** Generic columns

-- *** Type class

-- | Type class for statically typed, parsed column.
-- The type is represented by the attribute type.
class (DataTypeClass a, Typeable a, UVec.Unbox (ColumnRepr a),
  Typeable (ColumnRepr a), Typeable (ColumnExposed a),
  Eq (ColumnExposed a)) => ColumnDataType a where
  -- | Type family assigning a data representation type to the attribute type.
  type ColumnRepr a
  -- | Injective type family assigning a display type for the attribute type.
  type ColumnExposed a = e | e -> a
  -- | Creates a column from a list of the displayed types without checks.
  unsafeFromList :: [ColumnExposed a] -> Column a
  -- | A column invariant that's checked before creating the column.
  -- E.g. 'Natural Column' must not contain negative integers.
  columnInvariant :: ColumnExposed a -> Fallible ()

-- *** GenericColumn

data GenericColumn dtype repr exposed = GC {
  values :: UVec.Vector repr,
  exposure :: repr -> exposed,
  columnNameGen :: Maybe ColumnName
}

genColType :: DataTypeClass dtype => GenericColumn dtype repr exposed -> dtype
genColType col = staticType

instance (Eq exposed, UVec.Unbox repr) => Eq (GenericColumn dtype repr exposed) where
  a == b = (eqPrep a) == (eqPrep b)
    where
      eqPrep x = fmap (exposure x) $ UVec.toList (values x)

instance (ColumnDataType a, UVec.Unbox r, Eq e) => ColumnClass (GenericColumn a r e) where
  columnSize = values >>> UVec.length
  columnName = maybe (Left NoColumnName) Right . columnNameGen
  nameColumn' name c = c { columnNameGen = Just name }
  splitByFlags = splitByFlagsGen

instance ColumnDataType Numeric where
  type ColumnRepr Numeric = Double
  type ColumnExposed Numeric = Double
  unsafeFromList = unsafeFromVector . UVec.fromList
  columnInvariant _ = return ()

instance ColumnDataType Nominal where
  type ColumnRepr Nominal = Int
  type ColumnExposed Nominal = String
  unsafeFromList = unsafeNominalColumn
  columnInvariant _ = return ()

instance ColumnDataType Binary where
  type ColumnRepr Binary = Bool
  type ColumnExposed Binary = Bool
  unsafeFromList = unsafeFromVector . UVec.fromList
  columnInvariant _ = return ()

instance ColumnDataType Natural where
  type ColumnRepr Natural = Int
  type ColumnExposed Natural = Int
  unsafeFromList = unsafeFromVector . UVec.fromList
  columnInvariant x = if x >= 0
    then return ()
    else throwError $
      BrokenInvariant $ "Negative element while constructing NaturalColumn: " ++ show x

-- *** Column aliases
type Column a = GenericColumn a (ColumnRepr a) (ColumnExposed a)
type NumericColumn = GenericColumn Numeric Double Double
type NominalColumn = GenericColumn Nominal Int String
type BinaryColumn = GenericColumn Binary Bool Bool
type NaturalColumn = GenericColumn Natural Int Int

-- | Creates a column from a list of the displayed type.
-- E.g. a 'Column Numeric' can be created from a '[Double]'.
fromList :: (ColumnDataType dtype) => [ColumnExposed dtype] -> Fallible (Column dtype)
fromList xs = do
  -- checking whether the column is empty
  if null xs then throwError EmptyColumn else return ()
  -- checking the column invariant
  traverse columnInvariant xs
  return $ unsafeFromList xs

-- | Creates a list of the displayed types from a column.
-- E.g. a '[Double]' can be created from a 'Column Numeric'.
toList :: (ColumnDataType a) => Column a -> [ColumnExposed a]
toList col = fmap (exposure col) $ UVec.toList $ values col

-- | Creates a column from a 'Data.Vector.Unboxed.Vector' of the representation type
-- without checks.
unsafeFromVector :: (Eq (ColumnExposed dtype), Eq (ColumnRepr dtype), UVec.Unbox repr) =>
  UVec.Vector repr -> GenericColumn dtype repr repr
unsafeFromVector !vec = GC { values = vec, exposure = id, columnNameGen = Nothing }

-- | Creates a nominal column from a list of 'String's
-- without checks.
unsafeNominalColumn :: [String] -> Column Nominal
unsafeNominalColumn xs = nominalColumnFromCategories catsVec vals
  where
    vals = UVec.fromList $ fmap (\x -> Set.findIndex x cats) xs
    catsVec = Vec.fromList $ Set.toAscList cats
    -- creating a list
    cats = Set.fromList xs

-- | Creates a nominal column from a 'Vector' of nominal values and indexes marking that category
-- without checks.
nominalColumnFromCategories :: Vector String -> UVec.Vector Int -> Column Nominal
nominalColumnFromCategories cats v = GC {
  values = v,
  exposure = \i -> cats Vec.! i,
  columnNameGen = Nothing
}

-- | Creates the distinct representation values in a nominal column.
distinctValues :: NominalColumn -> [Int]
distinctValues = Set.toList . Set.fromList . UVec.toList . values

-- | Throws 'EmptyColumn' error when list is empty.
checkNonEmptyColumn :: [a] -> Fallible ()
checkNonEmptyColumn xs = if null xs
  then throwError EmptyColumn
  else return ()

-- ** DataColumn

-- | Dynamically typed column.
-- The column type could be numeric, nominal, binary, or natural.
data DataColumn = forall dtype repr exposed .
  (DataTypeClass dtype, ColumnDataType dtype,
   Typeable (GenericColumn dtype repr exposed),
   (ColumnExposed dtype) ~ exposed,
   (ColumnRepr dtype) ~ repr,
   PrettyPrintable exposed) =>
  DC (GenericColumn dtype repr exposed)

instance Eq DataColumn where
  (DC col1) == (DC col2) = case cast col1 of
    Just castedCol1 -> castedCol1 == col2
    Nothing -> False

instance ColumnClass DataColumn where
  columnSize (DC col) = columnSize col
  columnName (DC col) = columnName col
  nameColumn' name (DC col) = DC $ nameColumn' name col
  splitByFlags flags (DC col) = do
    (c1, c2) <- splitByFlags flags col
    return (DC c1, DC c2)

-- | The type of the dynamic 'DataColumn'.
columnType :: DataColumn -> DataType
columnType (DC col) = atRuntime $ genColType col

-- | Creates dynamically typed column from statically typed.
asDataColumn :: (DataTypeClass dtype, ColumnDataType dtype,
   Typeable (GenericColumn dtype repr exposed),
   (ColumnExposed dtype) ~ exposed,
   (ColumnRepr dtype) ~ repr,
   PrettyPrintable exposed) =>
  GenericColumn dtype repr exposed -> DataColumn
asDataColumn = DC

-- | Tries to creates statically typed column from dynamically typed.
asColumn :: (ColumnDataType dtype) =>
  DataColumn -> Fallible (Column dtype)
asColumn (DC col) = maybe (Left CannotCast) Right (cast col)


-- * Splitting by flags

splitByFlagsRaw :: [Bool] -> RawColumn -> Fallible (RawColumn, RawColumn)
splitByFlagsRaw flags col = do
  (c1, c2) <- splitByFlagsVec flags $ valuesRaw col
  return (setValuesRaw c1 col, setValuesRaw c2 col)

splitByFlagsGen :: UVec.Unbox r => [Bool] -> GenericColumn a r e ->
  Fallible (GenericColumn a r e, GenericColumn a r e)
splitByFlagsGen flags col = do
  (c1, c2) <- splitByFlagsUVec flags $ values col
  return $ (col { values = c1 }, col { values = c2 })

-- | Helper for column splitting.
splitByFlagsVec :: [Bool] -> Vector a -> Fallible (Vector a, Vector a)
splitByFlagsVec flags xs = if Vec.length xs1 /= 0 && Vec.length xs2 /= 0
    then Right (xs1, xs2)
    else Left EmptyColumn
  where
    (xs1, xs2) = (Vec.map snd *** Vec.map snd) $
      Vec.partition (\(flag, x) -> flag) $
      Vec.zip vecFlags xs
    vecFlags = Vec.fromList $ take (Vec.length xs) flags

-- | Helper for column splitting.
splitByFlagsUVec :: UVec.Unbox a => [Bool] -> UVec.Vector a -> Fallible (UVec.Vector a, UVec.Vector a)
splitByFlagsUVec flags xs = if UVec.length xs1 /= 0 && UVec.length xs2 /= 0
    then Right (xs1, xs2)
    else Left EmptyColumn
  where
    (xs1, xs2) = (UVec.map snd *** UVec.map snd) $
      UVec.partition (\(flag, x) -> flag) $
      UVec.zip vecFlags xs
    vecFlags = UVec.fromList $ take (UVec.length xs) flags

-- * Naming

-- | Names a column.
nameColumn :: ColumnClass c => ColumnName -> c -> Fallible c
nameColumn name c = do
  validName <- validateName name
  return $ nameColumn' validName c

-- | Validates a column name.
validateName :: ColumnName -> Fallible ColumnName
validateName name = if (Prelude.elem '_' name)
  then Left $ InvalidName $ "\"" ++ name ++ "\". Name cannot contain _"
  else (Right name)

-- | Assigns the name to the column.
copyName :: ColumnDataType b => Maybe ColumnName -> Column b -> Column b
copyName colName to = to { columnNameGen = colName }

-- * Transforming
-- | Mapping to columns by the exposed type.
mapColumnAtoB :: (ColumnDataType a, ColumnDataType b, ColumnExposed b ~ ColumnRepr b) =>
  (ColumnExposed a -> ColumnExposed b) -> Column a -> Column b
mapColumnAtoB f col = col { values = UVec.map (f . exposure col) (values col), exposure = id }

-- * Helpers for pretty printing

data PrettyPrintParams = PrettyPrintParams {
  numberOfDecimals :: Int
  , maxNumberOfLinesToShow :: Int
  , maxNumberOfColumnsInWindow :: Int
}

class PrettyPrintable a where
  prettyPrint :: a -> Reader PrettyPrintParams String

data PrettyPrintableData = forall a. (PrettyPrintable a) => PPData a

mkPrettyPrintable :: PrettyPrintable a => a -> PrettyPrintableData
mkPrettyPrintable = PPData

instance PrettyPrintable PrettyPrintableData where
  prettyPrint (PPData x) = prettyPrint x

dataColumnToPrettyPrintables :: DataColumn -> Vector PrettyPrintableData
dataColumnToPrettyPrintables (DC col) = columnToPrettyPrintables col

columnToPrettyPrintables :: (ColumnDataType a, PrettyPrintable e, UVec.Unbox r) =>
  GenericColumn a r e -> Vector PrettyPrintableData
columnToPrettyPrintables col =
  Vec.map (mkPrettyPrintable . exposure col) $ UVec.convert (values col)

