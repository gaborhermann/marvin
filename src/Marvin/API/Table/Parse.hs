{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-|
Module      : Marvin.API.Table.Parse
Description : Reading tables from CSV file, parsing tables from raw data.
-}
module Marvin.API.Table.Parse where

import Marvin.API.Table.DataType
import Marvin.API.Fallible
import Marvin.API.Table.Internal
import Marvin.API.Table.PrettyPrint

import qualified Data.Csv as Csv
import Data.Csv (Csv)
import Control.Monad.Except
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Control.Arrow
import Control.Monad.Except
import qualified Data.List as List

import Control.DeepSeq

import qualified Data.ByteString.Char8 as BS8 (pack, unpack)
import Data.Char (ord)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSLazy
import Data.ByteString.Internal (c2w,w2c)

import System.IO
import System.IO.Error
import Control.Exception
import Data.Maybe (isJust)

-- * Reading CSV

-- | Type for configuring whether CSV has header.
data HasHeader = HasHeader | NoHeader deriving Eq

-- | Type alias for cell delimiter in CSV
type CellDelimiter = Char

-- | Reads a file from a CSV creating a 'RawTable'
fromCsv :: CellDelimiter -> HasHeader -> FilePath -> IO (Fallible RawTable)
fromCsv cellDelim header path = handle readHandler $ withFile path ReadMode $ \h -> do
    input <- BSLazy.hGetContents h
    let !table = bsToRawTable input cellDelim header
    hClose h
    return table

-- | Reads a file
fromCsvWithScheme :: [DataType] -> CellDelimiter -> HasHeader -> FilePath ->
  IO (Fallible DataTable)
fromCsvWithScheme scheme cellDelim header path = do
  rawTable <- fromCsv cellDelim header path
  return $ rawTable >>= parseWithScheme scheme

-- ** Helpers

-- | Mapping 'IOError's during read Marvin 'Error's.
readHandler :: IOError -> IO (Fallible a)
readHandler e = return $
  throwError $ ReadError $ ioeGetErrorString e

-- | Creates a 'RawTable' from a lazy 'ByteString' with the CSV contents.
bsToRawTable :: BSLazy.ByteString -> CellDelimiter -> HasHeader ->
  Fallible RawTable
bsToRawTable input cellDelim header = do
    -- decoding CSV with cassava
    let decoded = Csv.decodeWith (Csv.defaultDecodeOptions {
                                   Csv.decDelimiter = fromIntegral (ord cellDelim)
                                 }) Csv.NoHeader input :: Either String Csv
    -- mapping cassava errors to Marvin errors
    rows <- either (throwError . ReadError) return decoded :: Fallible Csv
    -- checking whether CSV is empty.
    if Vec.length rows == 0
      then throwError $ ReadError "Empty CSV file."
      else return ()
    -- creating a table from rows
    fromCsvRows header rows

-- | Creates a 'RawTable' from CSV rows with or without a header.
fromCsvRows :: HasHeader -> Csv -> Fallible RawTable
fromCsvRows header rows = case header of
    HasHeader -> fromCsvRowsWithHeader (Vec.map BS8.unpack (Vec.head rows)) (Vec.tail rows)
    NoHeader -> rowsToRawTab rows

-- | Creates a 'RawTable' from CSV rows with a header.
fromCsvRowsWithHeader :: Vector String -> Csv -> Fallible RawTable
fromCsvRowsWithHeader header rows = do
  rawTable <- rowsToRawTab rows
  let cols = columnsVec rawTable
  namedCols <- Vec.sequence $ Vec.zipWith nameColumn header cols
  return $ unsafeFromCols namedCols

-- | Creates a 'RawTable' from CSV rows without a header.
rowsToRawTab :: Csv -> Fallible RawTable
rowsToRawTab csv = do
  cols <- rowsToCols csv
  return $ unsafeFromCols $ Vec.map fromVectorToRaw cols

-- * Parsing

-- | Parses a statically typed table from a 'RawTable'.
parseTable :: (ColumnDataType a, Csv.FromField (ColumnExposed a)) =>
  RawTable -> Fallible (Table a)
parseTable raw = do
  let cols = columnsVec raw
  parsedCols <- traverse parseColumn cols
  return $ unsafeFromCols parsedCols

-- | Parses a statically typed column from a 'RawTable'.
parseColumn :: (ColumnDataType a, Csv.FromField (ColumnExposed a)) =>
  RawColumn -> Fallible (Column a)
parseColumn (RawColumn (name, vals)) = maybe
  (Left CannotParseColumn)
  (fmap (copyName name) . fromList . Vec.toList)
  maybeParsed
  where
    maybeParsed = traverse (parseMaybe) vals

-- | Parses a dynamically typed column from a 'RawTable' and a given 'DataType'.
parseColumnAs :: DataType -> RawColumn -> Fallible DataColumn
parseColumnAs colType col = case colType of
  Numeric -> fmap DC (parseColumn col :: Fallible (Column Numeric))
  Nominal -> fmap DC (parseColumn col :: Fallible (Column Nominal))
  Binary -> fmap DC (parseColumn col :: Fallible (Column Binary))
  Natural -> fmap DC (parseColumn col :: Fallible (Column Natural))

-- | Parses a dynamically typed table from a 'RawTable' and given 'DataType's of the columns.
parseWithScheme :: [DataType] -> RawTable -> Fallible DataTable
parseWithScheme colTypes raw = do
  let cols = columnsVec raw
  let colTypesVec = Vec.fromList colTypes
  if Vec.length colTypesVec /= numberOfColumns raw
      then throwError $ RowLengthMismatch $ "While parsing with scheme. " ++
        "The number of the given types do not match the number of columns in the table."
      else return ()
  parsedCols <- sequence $ Vec.zipWith parseColumnAs colTypesVec cols
  return $ unsafeFromCols parsedCols

-- ** Smart parsing

-- | Parses a column by guessing the type.
-- Tries binary, natural, numeric and nominal in this order.
smartParseColumn :: RawColumn -> DataColumn
smartParseColumn col = either (\_ -> nomCol) id $ binCol `orElse` natCol `orElse` numCol
  where
    numCol = fmap DC (parseColumn col :: Fallible (Column Numeric))
    binCol = fmap DC (parseColumn col :: Fallible (Column Binary))
    nomCol = DC (parseNominalColumn col :: Column Nominal)
    natCol = fmap DC (parseColumn col :: Fallible (Column Natural))


-- | Parses a table by guessing the column types.
-- Tries for every column binary, natural, numeric and nominal in this order.
smartParseTable :: RawTable -> DataTable
smartParseTable = columnsVec >>> fmap smartParseColumn >>> unsafeFromCols

-- ** Helpers

-- | Parses nominal column.
parseNominalColumn :: RawColumn -> Column Nominal
parseNominalColumn col =
  -- we can avoid parse check as nominal can be parsed for sure (it is represented by string)
  let Right parsed = parseColumn col in parsed

-- | Parses a field using cassava.
parseMaybe :: Csv.FromField a => BS.ByteString -> Maybe a
parseMaybe = Csv.parseField >>> Csv.runParser >>> either (const Nothing) Just

instance Csv.FromField Bool where
  parseField x = intParser >>= iToB
    where
      intParser = Csv.parseField x :: Csv.Parser Int
      iToB 0 = return False
      iToB 1 = return True
      iToB _ = fail "cannot parse Bool"
