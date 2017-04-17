module Marvin.API (
  -- * Fallible type
  Fallible (..)
  , Error (..)
  -- * Data types
  , DataType (..)
  , Numeric
  , Nominal
  , Natural
  , Binary

  -- * DataColumn
  -- ** Types
  , Column
  , NumericColumn
  , NominalColumn
  , BinaryColumn
  , NaturalColumn
  , RawColumn
  -- ** Constructors
  , fromList
  , toList
  , fromListToRaw
  , rawToList
  -- ** Dynamic DataColumn
  , DataColumn
  , asDataColumn
  , asColumn
  , columnType
  -- ** Naming
  , nameColumn
  -- ** Type classes and type families
  , ColumnClass
  , ColumnDataType
  , ColumnRepr
  , ColumnExposed
  -- * Useful aliases
  , ColumnName

  -- * DataTable
  -- ** Type classes and type families
  , TableClass
  , ColumnType
  -- * Types
  , Table
  , NumericTable
  , NominalTable
  , BinaryTable
  , NaturalTable
  , RawTable
  -- ** Constructors
  , emptyTable
  , fromRows
  , fromColumns
  , fromRowsToRaw
  -- ** Querying
  , columns
  , columnNames
  , numberOfColumns
  , numberOfRows
  -- ** ColumnId
  , ColumnId
  , byName
  , byIndex
  , lastColumn
  -- ** Operations
  , getColumn
  , removeColumn
  , featureUnion
  , appendColumn
  , selectTargetVariable
  -- ** Dynamic DataTable
  , DataTable
  , asTable
  , suitableColumns
  , numericColumns
  , nominalColumns
  , binaryColumns
  , naturalColumns
  , getColumnOfType
  , columnTypes
  -- ** Conversions
  , binaryToNumericColumn
  , binaryToNumeric
  , naturalToNumericColumn
  , naturalToNumeric

  -- * Preprocessing functions
  , minMaxScaleColumn
  , minMaxScale

  , standardizeColumn
  , standardize

  , oneHotEncodeColumn
  , oneHotEncode

  , trainTestSplit

  -- * Parse
  -- ** Header
  , HasHeader (..)
  -- ** Importing
  , fromCsv
  , fromCsvWithScheme
  -- ** Parsing
  , parseTable
  , parseColumn
  , parseColumnAs
  , parseWithScheme
  -- ** Smart parsing
  , smartParseColumn
  , smartParseTable

  -- * Train and predict
  , module Marvin.API.Meta.Model
  -- * Pipeline
  , module Marvin.API.Meta.Pipeline
  , module Marvin.API.Meta.PipelineModel

  -- * Evaluation
  , evaluate
  , fitAndEvaluate
  -- ** Types
  , Evaluator
  , EvalPrediction
  -- ** Methods
  , module Marvin.API.EvaluationMetrics
) where

import Marvin.API.Fallible
import Marvin.API.Table.DataType
import Marvin.API.Table.Internal

import Marvin.API.Table.PrettyPrint
import Marvin.API.Table.Parse
import Marvin.API.Meta.Model
import Marvin.API.Meta.Evaluator
import Marvin.API.Meta.Pipeline
import Marvin.API.Meta.PipelineModel

import Marvin.API.Preprocess.OneHotEncode
import Marvin.API.Preprocess.FeatureScaling
import Marvin.API.EvaluationMetrics

import Marvin.API.Table.TrainTestSplit
