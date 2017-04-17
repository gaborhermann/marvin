{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Marvin.API.Algorithms.NaiveBayes (
  NaiveBayes (..)
  , defaultNaiveBayes

  , BernoulliModel
  , threshold
  , setThreshold
  , probabilityOfTrue
  , trueProbabilities
  , falseProbabilities
) where

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as UVec
import qualified Data.Vector as Vec

import Data.List
import Control.Monad.Except

import Control.Arrow
import Marvin.API.Table.Internal
import Marvin.API.Meta.Model
import Marvin.API.Fallible

-- * Parameters

-- | Parameters.
data NaiveBayes = NaiveBayes {
  smoothingValue :: Double
}

-- | Default parameters.
defaultNaiveBayes = NaiveBayes { smoothingValue = 1.0 }

-- * Model

-- | Resulting model.
data BernoulliModel = BM {
  probabilityOfTrue_ :: Double
  , trueProbabilities_ :: UVec.Vector Double
  , falseProbabilities_ :: UVec.Vector Double
  , threshold_ :: Double
}

-- | Retrieves the threshold to decide 'True' or 'False' based on probability.
threshold = threshold_
-- | Sets the threshold that decides 'True' or 'False' based on probability.
setThreshold t model = model { threshold_ = t }

-- | Retrieves the probability that the target is 'True'.
probabilityOfTrue = probabilityOfTrue_
-- | Retrieves the conditional probabilities of 'True' target.
trueProbabilities = UVec.toList . trueProbabilities_
-- | Retrieves the conditional probabilities of 'False' target.
falseProbabilities = UVec.toList . falseProbabilities_

instance Show BernoulliModel where
  show model = "\n\nBernoulliModel\n\n" ++
    "\tp(y = 1) = " ++ show (probabilityOfTrue model) ++ "\n" ++
    "\tp(x_i = 1 | y = 0) = " ++ show (falseProbabilities model) ++ "\n" ++
    "\tp(x_i = 1 | y = 1) = " ++ show (trueProbabilities model) ++ "\n"

instance Estimator NaiveBayes (BinaryTable, BinaryColumn) where
  type ResultingModel NaiveBayes (BinaryTable, BinaryColumn) = BernoulliModel
  fit = fit'

fit' :: NaiveBayes -> (BinaryTable, BinaryColumn) -> Fallible BernoulliModel
fit' nbParams (bt, bc) = do
  ensureNonNegative
    (InvalidAlgorithmParameter "NaiveBayes" "smoothing value must be non-negative")
      $ smoothingValue nbParams
  ensureTableColSizeMatch "while training Naive Bayes" bt bc
  return $ fitNB nbParams featureMtx targetVar
  where
    featureMtx = tableToRows bt
    targetVar = UVec.toList $ values $ bc

tableToRows = Vec.toList . transposeVectorVectorU . Vec.map values . columnsVec

dimCheck :: BernoulliModel -> BinaryTable -> Fallible ()
dimCheck bernoulliModel binTable =
  if (UVec.length (trueProbabilities_ bernoulliModel) == numberOfColumns binTable)
    then return ()
    else throwError $ RowLengthMismatch "when attempting to make prediction based on NaiveBayes"

instance Predictor BernoulliModel where
  type Testing BernoulliModel = BinaryTable
  type Prediction BernoulliModel = BinaryColumn
  predict m t = do
    dimCheck m t
    return $ predict' m t

predict' model xs = unsafeFromList $
  fmap (\x -> x > threshold model) $
  fmap (predictProbaForOne model) rows
    where rows = tableToRows xs

predictProbaForOne :: BernoulliModel -> UVec.Vector Bool -> Double
predictProbaForOne model xs = pX_Y1 / (pX_Y1 + pX_Y0)
  where
    pXi_Y phis = UVec.zipWith (\x phi -> if x then phi else (1 - phi)) xs phis
    pXi_Y1 = pXi_Y $ trueProbabilities_ model
    pXi_Y0 = pXi_Y $ falseProbabilities_ model
    pX_Y1 = pY1 * (UVec.product pXi_Y1)
    pX_Y0 = (1 - pY1) * (UVec.product pXi_Y0)
    pY1 = probabilityOfTrue model


fitNB :: NaiveBayes -> [UVec.Vector Bool] -> [Bool] -> BernoulliModel
fitNB nb x y = BM {
    probabilityOfTrue_ = fromIntegral (length trueVecs) / fromIntegral m
    , trueProbabilities_ = probs trueVecs
    , falseProbabilities_ = probs falseVecs
    , threshold_ = 0.5
  }
  where
    n = UVec.length $ head x
    m = length x
    a = smoothingValue nb
    (trueVecs, falseVecs) =
      partition (\(xi,yi) -> yi) $
      zip x y
    probs xs = freqs (length xs) $ map fst xs
    freqs ny =
      map (UVec.map (\x -> if x then (1 :: Int) else 0)) >>>
      foldl1 (UVec.zipWith (+)) >>>
      UVec.map (\nyi -> ((fromIntegral nyi) + a) / ((fromIntegral ny) + a*(fromIntegral n)))
