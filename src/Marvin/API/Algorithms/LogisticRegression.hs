{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Marvin.API.Algorithms.LogisticRegression (
  LogisticRegression (..)
  , defaultLogisticRegression

  , LogisticRegressionModel
  , threshold
  , setThreshold
  , coefficients
  , intercept
) where

import qualified Numeric.LinearAlgebra as LA (toList)

import qualified Marvin.API.Algorithms.Internal.GradientDescent as GD
import Marvin.API.Algorithms.Internal.GradientDescent hiding (learningRate, lambda, addIntercept)

import Marvin.API.Meta.Model
import Marvin.API.Table.Internal
import Marvin.API.Fallible

-- * Parameters

-- | Parameters.
data LogisticRegression = LogisticRegression {
  learningRate :: Double -- ^ Coefficient of the gradient when updating the model.
  , numberOfIterations :: Int -- ^ Number of iterations.
  , lambda :: Double -- ^ Regularization coefficient.
  , addIntercept :: Bool -- ^ Whether to add a constant 1 column before training.
}

-- | Default parameters.
defaultLogisticRegression = LogisticRegression {
  learningRate = 0.001
  , numberOfIterations = 10
  , lambda = 0.1
  , addIntercept = True
}

toGradDesc :: LogisticRegression -> GradientDescent
toGradDesc logReg = defaultGradientDescent {
  numIter = numberOfIterations logReg
  , GD.cost = logisticRegressionCost
  , GD.learningRate = learningRate logReg
  , GD.addIntercept = addIntercept logReg
  , GD.lambda = lambda logReg
}

-- * Model

-- | Resulting model.
newtype LogisticRegressionModel = LogRegModel (Double, LinearModel)

-- | Retrieves the threshold to decide 'True' or 'False' based on probability.
threshold (LogRegModel (t, _)) = t
-- | Sets the threshold that decides 'True' or 'False' based on probability.
setThreshold t (LogRegModel (_, linear)) = LogRegModel (t, linear)

-- | Retrieves a list of the coefficient.
coefficients :: LogisticRegressionModel -> [Double]
coefficients (LogRegModel (_, linearModel)) = LA.toList $ coefs linearModel

-- | Retrieves the constant to be added.
intercept :: LogisticRegressionModel -> Double
intercept (LogRegModel (_, linearModel)) = intercept_ linearModel

instance Show LogisticRegressionModel where
  show (LogRegModel linear) = "Logistic regression model:\n\n" ++ show linear

instance Estimator LogisticRegression (NumericTable, BinaryColumn) where
  type ResultingModel LogisticRegression (NumericTable, BinaryColumn) =
    LogisticRegressionModel
  fit logReg (train, target) = do
    ensurePositive
      (InvalidAlgorithmParameter "LogisticRegression" "numberOfIterations must be positive")
        $ numberOfIterations logReg
    ensurePositive
      (InvalidAlgorithmParameter "LogisticRegression" "learning rate must be positive")
        $ learningRate logReg
    ensureNonNegative
      (InvalidAlgorithmParameter "LogisticRegression" "lambda must be non-negative")
        $ lambda logReg
    let numTarget = binaryToNumericColumn target
    ensureTableColSizeMatch "while training logistic regression" train numTarget
    let linearModel = fit' (toGradDesc logReg) (train, numTarget)
    let defaultThreshold = 0.5
    return $ LogRegModel (defaultThreshold, linearModel)

instance Predictor LogisticRegressionModel where
  type Testing LogisticRegressionModel = NumericTable
  type Prediction LogisticRegressionModel = BinaryColumn
  predict (LogRegModel (thresh, model)) testingTable = do
    numericPredictions <- probaPredict model testingTable
    return $ mapColumnAtoB (\x -> if x < thresh then False else True) numericPredictions

probaPredict model testingTable = do
  numericPredictions <- predict' model testingTable
  return $ mapColumnAtoB sigmoid numericPredictions
