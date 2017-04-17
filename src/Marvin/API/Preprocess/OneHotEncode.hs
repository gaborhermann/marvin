{-|
Module      : Marvin.API.Preprocess.OneHotEncode
Description : Encoding nominal features to binaries.
-}
module Marvin.API.Preprocess.OneHotEncode where

import qualified Data.Vector as Vec

import Marvin.API.Fallible
import Marvin.API.Table.Internal

-- | Encodes a 'NominalColumn' to a 'BinaryTable'
-- where each binary column represents a nominal value.
-- An element in a certain binary column is true if it's the column representing the corresponding
-- nominal value.
oneHotEncodeColumn :: NominalColumn -> NominalColumn -> BinaryTable
oneHotEncodeColumn fitCol testCol = unsafeFromCols $ Vec.fromList $ binCols
  where
    binCols = fmap (\val -> nameColumn' (mkName val) (mapColumnAtoB (== val) testCol)) $ nomVals
    nomVals = map expose $ distinctValues fitCol
    expose = exposure fitCol
    colName = either (const "") id $ columnName fitCol
    mkName name = colName ++ "_is_" ++ name

-- | Encodes a 'NominalTable' to a 'BinaryTable'.
-- An element in a certain binary column is true if it's the column representing the corresponding
-- nominal value.
oneHotEncode :: NominalTable -> NominalTable -> Fallible BinaryTable
oneHotEncode train test = case transformTable (\col -> columns . (oneHotEncodeColumn col)) of
    Left (ColumnNameConflict _) ->
          transformTable (\col -> fmap (copyName Nothing) . columns . (oneHotEncodeColumn col))
    x -> x
    where
      transformTable f = transformEveryColumn "When using oneHotEncode." f train test
