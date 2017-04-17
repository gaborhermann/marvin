{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : Marvin.API.Meta.Model
Description : Types classes for training and predicting.
-}
module Marvin.API.Meta.Model where

import Marvin.API.Fallible

-- | Type class for training.
class Estimator algo train where
  -- | Type family that assigns a model type to given algorithm and training data types.
  type ResultingModel algo train
  -- | Training a model.
  fit ::
    algo -- ^ Algorithm
    -> train -- ^ Training data
    -> Fallible (ResultingModel algo train) -- ^ Resulting model

-- | Type class for predicting.
class Predictor m where
  -- | Type family that assigns a testing data type to a model.
  -- Values can be predicted from this type with a given model.
  type Testing m
  -- | Type family that assigns the prediction type to a model.
  type Prediction m
  predict ::
    m -- ^ Model
    -> Testing m -- ^ Data to give predictions to
    -> Fallible (Prediction m) -- ^ Predictions

