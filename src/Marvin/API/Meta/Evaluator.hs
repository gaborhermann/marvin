{-# LANGUAGE TypeFamilies #-}
{-|
Module      : Marvin.API.Meta.Evaluator
Description : Evaluating a model.
-}
module Marvin.API.Meta.Evaluator where

import Marvin.API.Fallible
import Marvin.API.Meta.Model

import Control.Arrow

-- | Type class for an evaluation metric.
class Evaluator metric where
  -- | Type family assigning a prediction type to an evaluation metric.
  -- Models with this type can be evaluated by a given metric.
  type EvalPrediction metric
  evaluate' ::
    metric -- ^ Evaluation metric.
    -> (EvalPrediction metric) -- ^ Column of real values.
    -> (EvalPrediction metric) -- ^ Column of predicted values.
    -> Fallible Double -- ^ Result of evaluation.

-- | Evaluates a model with an evaluation metric.
evaluate :: (Predictor model, Evaluator evaluator,
  EvalPrediction evaluator ~ Prediction model) =>
  model -- ^ Model.
  -> evaluator -- ^ Evaluation metric.
  -> Testing model -- ^ Testing data.
  -> Prediction model -- ^ Real values assigned to testing data.
  -> Fallible Double -- ^ Result of evaluation.
evaluate model evalMetric testData real = do
  predicted <- predict model testData
  evaluate' evalMetric real predicted

-- | Trains a model with a given algorithm and evaluates it with a given metric.
fitAndEvaluate :: (Predictor m, Evaluator eval, Estimator algo train,
    ResultingModel algo train ~ m,
    train ~ (Testing m, Prediction m),
    EvalPrediction eval ~ Prediction m) =>
  algo -> eval -> (Testing m, Prediction m) -> (Testing m, Prediction m) -> Fallible Double
fitAndEvaluate algorithm evaluator train (testX, testY) = do
  model <- fit algorithm train
  evalMetric <- evaluate model evaluator testX testY
  return evalMetric
