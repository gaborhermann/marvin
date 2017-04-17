{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
Module      : Marvin.API.Meta.PipelineModel
Description : Chained model.
-}
module Marvin.API.Meta.PipelineModel (
  PipelineModel
  , getModel
  , askModel
) where

import Marvin.API.Meta.Pipeline
import Marvin.API.Fallible
import Marvin.API.Meta.Model

-- | A model combined with a pipeline.
newtype PipelineModel model input = PM (model, input -> Fallible (Testing model))

-- | Retrieves the underlying model of a 'PipelineModel'.
getModel :: PipelineModel model input -> model
getModel (PM (model, _)) = model

-- | Performs a 'predict'-like operation on the underlying model of a 'PipelineModel',
-- using the input of the pipeline instead of the expected model input.
-- This is useful for using a 'PipelineModel' as if it were a normal model with different input type.
askModel :: PipelineModel model input -> (model -> (Testing model) -> Fallible result) ->
  input -> Fallible result
askModel (PM (model, preproc)) f input = do
  prepared <- preproc input
  f model prepared

-- Supervised version of Estimator generalization.
instance (Estimator algo (train, target), Testing (ResultingModel algo (train, target)) ~ train)
  => Estimator algo (Pipeline input train, input, target) where
  type ResultingModel algo (Pipeline input train, input, target) =
    PipelineModel (ResultingModel algo (train, target)) input
  fit algo (pipe, input, target) = do
    x <- preproc input
    model <- fit algo (x, target)
    return $ PM (model, preproc)
    where
      preproc = (fitThenRun pipe) input

-- Unsupervised version of Estimator generalization.
instance (Estimator algo train, Testing (ResultingModel algo train) ~ train) =>
  Estimator algo (Pipeline input train, input) where
  type ResultingModel algo (Pipeline input train, input) =
    PipelineModel (ResultingModel algo train) input
  fit algo (pipe, input) = do
    x <- preproc input
    model <- fit algo x
    return $ PM (model, preproc)
    where
      preproc = (fitThenRun pipe) input

instance (Predictor m) => Predictor (PipelineModel m i) where
  type Testing (PipelineModel m i) = i
  type Prediction (PipelineModel m i) = Prediction m
  predict = predictPipeline

predictPipeline :: (Predictor m) => PipelineModel m i -> i -> Fallible (Prediction m)
predictPipeline (PM (model, preproc)) input = do
  test <- preproc input
  predict model test

