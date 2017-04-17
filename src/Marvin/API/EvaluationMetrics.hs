{-# LANGUAGE TypeFamilies #-}
{-|
Module      : Marvin.API.EvaluationMetric
Description : Metrics used for evaluation.
-}
module Marvin.API.EvaluationMetrics (
  Accuracy (..)
  , MeanSquaredError (..)
  , RootMeanSquaredError (..)
  , Precision (..)
  , Recall (..)
) where

import Marvin.API.Meta.Evaluator
import Marvin.API.Table.Column
import Marvin.API.Fallible
import Marvin.API.Meta.Model
import Marvin.API.Table.DataType

import Data.Vector (Vector)
import qualified Data.Vector.Unboxed as UVec

-- * Accuracy

data Accuracy = Accuracy

accuracy :: PairwiseMetric Binary
accuracy = PM {
  prepare = \real predicted -> if (real == predicted) then 1 else 0
  , aggregate = average
}

instance Evaluator Accuracy where
  type EvalPrediction Accuracy = BinaryColumn
  evaluate' Accuracy = evaluatePairwise accuracy

-- * Mean squared error

data MeanSquaredError = MeanSquaredError

meanSqError :: PairwiseMetric Numeric
meanSqError = PM {
  prepare = \real predicted -> (real - predicted)^2
  , aggregate = average
}

instance Evaluator MeanSquaredError where
  type EvalPrediction MeanSquaredError = NumericColumn
  evaluate' MeanSquaredError = evaluatePairwise meanSqError

-- * Root mean squared error

data RootMeanSquaredError = RootMeanSquaredError

rootMeanSqError :: PairwiseMetric Numeric
rootMeanSqError = meanSqError {
  aggregate = sqrt . (aggregate meanSqError)
}

instance Evaluator RootMeanSquaredError where
  type EvalPrediction RootMeanSquaredError = NumericColumn
  evaluate' RootMeanSquaredError = evaluatePairwise rootMeanSqError

-- * Precision

data Precision = Precision

precision :: [Bool] -> [Bool] -> Double
precision real pred =
  fromIntegral truePos / fromIntegral truePosFalsePos
  where
    xs = zip real pred
    truePosFalsePos = length $ filter snd xs
    truePos = length $ filter (\(r, p) -> r && p) xs

instance Evaluator Precision where
  type EvalPrediction Precision = BinaryColumn
  evaluate' Precision real pred = return $
    precision (toList real) (toList pred)

-- * Recall

data Recall = Recall

recall :: [Bool] -> [Bool] -> Double
recall real pred =
  fromIntegral truePos / fromIntegral truePosFalseNeg
  where
    xs = zip real pred
    truePosFalseNeg = length $ filter fst xs
    truePos = length $ filter (\(r, p) -> r && p) xs

instance Evaluator Recall where
  type EvalPrediction Recall = BinaryColumn
  evaluate' Recall real pred = return $
    recall (toList real) (toList pred)

-- * Pairwise metric

-- | Alias for preparing evaluation element-wise.
type PairwisePrepare a =
  ColumnExposed a    -- ^ real
  -> ColumnExposed a -- ^ predicted
  -> Double

-- | Alias for aggregating the prepared elements.
type PairwiseAggregate =
  [Double] -> Double

-- | Type for pairwise evaluation.
data PairwiseMetric a = PM {
  prepare :: PairwisePrepare a
  , aggregate :: PairwiseAggregate
}

-- | Evaluation with a pairwise metric.
evaluatePairwise :: (ColumnDataType a) =>
  PairwiseMetric a
  -> Column a -- ^ real
  -> Column a -- ^ predicted
  -> Fallible Double
evaluatePairwise pairwise realCol predCol = return $ aggregate pairwise $
  zipWith (prepare pairwise) (exposedVals realCol) (exposedVals predCol)
  where
    exposedVals col = map (exposure col) $ UVec.toList $ values col

average :: PairwiseAggregate
average xs = sum xs / fromIntegral (length xs)