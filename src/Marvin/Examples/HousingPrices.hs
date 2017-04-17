{-# LANGUAGE Arrows #-}
module Marvin.Examples.HousingPrices where

import Control.Arrow
import Data.Traversable

import Marvin.API
import Marvin.API.Algorithms.LinearRegression

import System.Environment

linRegParams = LinearRegression {
  learningRate = 0.01,
  numberOfIterations = 1000,
  lambda = 0.1,
  addIntercept = True
}

housing housingPath = do
  Right csv <- fromCsv ',' HasHeader housingPath
  split <- trainTestSplit 0.8 csv
  let lambdas = [0.01, 0.1, 0.2, 0.3, 0.4, 1, 10]
  let msesByLambdas = do
        (train, test) <- split
        mses <- for lambdas $ \l ->
          (fitThenRun (evalLinReg linRegParams { lambda = l })) train test
        return $ zip lambdas mses
  return msesByLambdas

evalLinReg :: LinearRegression -> Pipeline RawTable Double
evalLinReg linReg = proc raw -> do
  numeric <- preproc -< raw
  (x, y) <- inject -< selectTargetVariable (byName "MEDV") numeric
  mse <- transform (fitAndEvaluate linReg MeanSquaredError) -< (x, y)
  returnA -< mse

preproc :: Pipeline RawTable NumericTable
preproc = proc x -> do
  let table = smartParseTable x
  let bins = binaryColumns table
  let nums = numericColumns table
  let noms = nominalColumns table
  let nats = naturalColumns table
  encodedNoms <- transform oneHotEncode -< noms
  allNums <- inject -< featureUnion [nums, naturalToNumeric nats]
  standardized <- transform standardize -< allNums
  uni <- inject -< featureUnion [binaryToNumeric encodedNoms, binaryToNumeric bins, standardized]
  returnA -< uni
