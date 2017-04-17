{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : Marvin.API.Fallible
Description : Types for error handling.
-}
module Marvin.API.Fallible where

import Control.Monad.Except

-- | Type representing different kinds of Marvin errors.
data Error =
  Failure String
  | RowLengthMismatch String
  | ColumnSizeMismatch String
  | ColumnNameConflict String
  | NoColumnName
  | NoSuchColumn String
  | CannotCast
  | CannotParseColumn
  | ReadError String
  | InvalidName String
  | EmptyTable String
  | InvalidAlgorithmParameter String String
  | EmptyColumn
  | BrokenInvariant String
  deriving (Eq)

instance Show Error where
  show e = "Error: " ++ case e of
    Failure msg -> msg
    RowLengthMismatch msg ->
      "Row length mismatch. " ++ msg ++
      " Make sure that all row has the same size in a table."
    ColumnSizeMismatch msg ->
      "Column size mismatch. " ++ msg ++
      " Make sure that all column has the same size in a table."
    ColumnNameConflict msg ->
      "Column name conflict. " ++ msg ++
      " Make sure that there are no two columns with the same name in a table"
    NoColumnName ->
      "There is no name available for the column."
    NoSuchColumn msg ->
      "There is no column with the requested id: " ++ msg ++ "."
    CannotCast ->
      "Cannot cast object. Are you trying to use a DataTable or DataColumn as a wrong type?"
    CannotParseColumn ->
      "Cannot parse column. Is the column in the right format?"
    ReadError msg ->
      "Cannot read file: " ++ msg
    InvalidName msg ->
      "Invalid column name: " ++ msg
    EmptyTable msg ->
      "The table is empty. " ++ msg
    InvalidAlgorithmParameter algo msg ->
      "Invalid algorithm parameter, while trying to fit " ++ algo ++ ". " ++ msg
    EmptyColumn ->
      "Empty column. Columns must contain at least one element."
    BrokenInvariant msg ->
      "Broken column invariant. " ++ msg

-- | Type synonym for monadic handling of 'Error's.
type Fallible = Either Error

-- | Ensures a property by throwing an 'Error' if the value does not satisfy the property.
ensureProperty :: (a -> Bool) -> Error -> a -> Fallible ()
ensureProperty p error x = if (p x) then return () else throwError error

-- | Ensures that a value is positive, throws an 'Error' if not.
ensurePositive :: (Ord a, Num a) => Error -> a -> Fallible ()
ensurePositive error = ensureProperty (> 0) error

-- | Ensures that a value is positive.
ensureNonNegative :: (Ord a, Num a) => Error -> a -> Fallible ()
ensureNonNegative error = ensureProperty (>= 0) error

-- | Takes two 'Fallible' values and returns the first if it is not an 'Error',
-- otherwise returns the second.
orElse :: Fallible a -> Fallible a -> Fallible a
orElse x y = either (\a -> y) Right x

-- | Converting Fallible to a 'Maybe'.
fallibleToMaybe :: Fallible a -> Maybe a
fallibleToMaybe = either (const Nothing) Just

-- | Showing a 'Fallible' value without displaying the 'Right' and 'Left' constructors of 'Either'.
showFallible :: (Show a) => Fallible a -> String
showFallible (Left e) = show e
showFallible (Right x) = show x

