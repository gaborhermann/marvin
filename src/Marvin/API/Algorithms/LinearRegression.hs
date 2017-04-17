{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Marvin.API.Algorithms.LinearRegression (
  LinearRegression (..)
  , defaultLinearRegression

  , LinearRegressionModel
  , coefficients
  , intercept
) where

import qualified Numeric.LinearAlgebra as LA
import Numeric.LinearAlgebra.HMatrix

import qualified Marvin.API.Algorithms.Internal.GradientDescent as GD
import Marvin.API.Algorithms.Internal.GradientDescent hiding (learningRate, lambda, addIntercept)

import Marvin.API.Meta.Model
import Marvin.API.Table.DataType
import Marvin.API.Table.Internal
import Marvin.API.Fallible

import Control.Arrow
import Control.Monad.Except
import qualified Data.Vector as Vec

-- * Parameters

-- | Parameters.
data LinearRegression = LinearRegression {
  learningRate :: Double -- ^ Coefficient of the gradient when updating the model.
  , numberOfIterations :: Int -- ^ Number of iterations.
  , lambda :: Double -- ^ Regularization coefficient.
  , addIntercept :: Bool -- ^ Whether to add a constant 1 column before training.
}

-- | Default parameters.
defaultLinearRegression = LinearRegression {
  learningRate = 0.001
  , numberOfIterations = 10
  , lambda = 0.1
  , addIntercept = True
}

toGradDesc :: LinearRegression -> GradientDescent
toGradDesc linReg = defaultGradientDescent {
  numIter = numberOfIterations linReg
  , GD.cost = linearRegressionCost
  , GD.learningRate = learningRate linReg
  , GD.addIntercept = addIntercept linReg
  , GD.lambda = lambda linReg
}

-- * Model

-- | Resulting model.
newtype LinearRegressionModel = LinRegModel LinearModel

-- | Retrieves a list of the coefficient.
coefficients :: LinearRegressionModel -> [Double]
coefficients (LinRegModel linearModel) = LA.toList $ coefs linearModel

-- | Retrieves the constant to be added.
intercept :: LinearRegressionModel -> Double
intercept (LinRegModel linearModel) = intercept_ linearModel

instance Show LinearRegressionModel where
  show (LinRegModel linear) = "Linear regression model:\n\n" ++ show linear

instance Estimator LinearRegression (NumericTable, NumericColumn) where
  type ResultingModel LinearRegression (NumericTable, NumericColumn) =
    LinearRegressionModel
  fit linReg (train, target) = do
    ensurePositive
      (InvalidAlgorithmParameter "LinearRegression" "numberOfIterations must be positive")
        $ numberOfIterations linReg
    ensurePositive
      (InvalidAlgorithmParameter "LinearRegression" "learning rate must be positive")
        $ learningRate linReg
    ensureNonNegative
      (InvalidAlgorithmParameter "LinearRegression" "lambda must be non-negative")
        $ lambda linReg
    ensureTableColSizeMatch "while training linear regression" train target
    let linearModel = fit' (toGradDesc linReg) (train, target)
    return $ LinRegModel linearModel

instance Predictor LinearRegressionModel where
  type Testing LinearRegressionModel = NumericTable
  type Prediction LinearRegressionModel = NumericColumn
  predict (LinRegModel model) = predict' model

unsafeFit :: LinearRegression -> FeatureMatrix -> TargetVariable -> ModelParameters
unsafeFit linReg = gradientDescent (toGradDesc linReg)
