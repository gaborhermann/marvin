{-|
Module      : Marvin.API.Table.DataType
Description : Basic attribute types. Both static and dynamic.
-}
module Marvin.API.Table.DataType where

-- | Dynamic attribute type.
data DataType = Binary | Nominal | Numeric | Natural deriving (Eq, Show)

-- | Typeclass for static attribute types.
class DataTypeClass a where
  atRuntime :: a -> DataType
  staticType :: a

-- | Static attribute type for nominal (categorical) data.
data Nominal = Nom deriving (Eq, Show)
-- | Static attribute type for numeric (continuous, floating point) data.
data Numeric = Num deriving (Eq, Show)
-- | Static attribute type for binary (boolean) data.
data Binary = Bin deriving (Eq, Show)
-- | Static attribute type for natural data (non-negative integers).
data Natural = Nat deriving (Eq, Show)

instance DataTypeClass Nominal where
  atRuntime _ = Nominal
  staticType = Nom

instance DataTypeClass Numeric where
  atRuntime _ = Numeric
  staticType = Num

instance DataTypeClass Binary where
  atRuntime _ = Binary
  staticType = Bin

instance DataTypeClass Natural where
  atRuntime _ = Natural
  staticType = Nat
