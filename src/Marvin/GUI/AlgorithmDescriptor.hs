{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Arrows #-}
module Marvin.GUI.AlgorithmDescriptor where

import Control.Arrow

import Marvin.API.Table.DataType
import Marvin.API.Fallible
import Marvin.API.Table.Internal
import Marvin.API
import qualified Marvin.API.Algorithms.LinearRegression as LinReg
import qualified Marvin.API.Algorithms.LogisticRegression as LogReg
import Marvin.API.Algorithms.LinearRegression (LinearRegression)
import Marvin.API.Algorithms.LogisticRegression (LogisticRegression)
import Marvin.API.Algorithms.NaiveBayes

import Data.Map as Map

algorithms :: [AlgorithmDescriptor]
algorithms = [toAlgoDesc linRegAlgoDesc, toAlgoDesc logRegAlgoDesc, toAlgoDesc naiveBayesAlgoDesc]

maxDouble = fromIntegral (maxBound :: Int)
-- parameters

data Parameter =
  DoubleParam Double (Maybe (Double, Double)) Double
  | IntParam Int (Maybe (Int, Int))
  | BoolParam Bool
  deriving Show
type Parameters = Map String Parameter

-- show type

class ShowType a where
  showType :: a -> String

instance ShowType Numeric where
  showType _ = "numeric"

instance ShowType Binary where
  showType _ = "binary"

data AlgorithmDesc algo features target = AlgorithmDesc {
  name :: String
  , desc :: String
  , expectedFeatures :: features
  , expectedTarget :: target
  , defaultParams :: algo
  , toParameters :: algo -> Parameters
  , fromParameters :: Parameters -> algo
  , prepareData :: Pipeline (DataTable, DataColumn) (Table features, Column target)
  , runAlgo :: algo -> Pipeline (Table features, Column target) String
}

-- algo desc

data AlgorithmDescriptor = forall a f t. (ShowType f, ShowType t) => AD (AlgorithmDesc a f t)

instance Show AlgorithmDescriptor where
  show (AD x) = name x

defaultAlgoParams :: AlgorithmDescriptor -> Parameters
defaultAlgoParams (AD x) = (toParameters x) (defaultParams x)

toAlgoDesc :: (ShowType f, ShowType t) => AlgorithmDesc a f t -> AlgorithmDescriptor
toAlgoDesc = AD

runAlgorithm :: AlgorithmDescriptor -> Parameters -> Pipeline (DataTable, DataColumn) String
runAlgorithm (AD algoDesc) params = proc (x, y) -> do
    (xSuitable, ySuitable) <- prepareData algoDesc -< (x, y)
    output <- (runAlgo algoDesc) algo -< (xSuitable, ySuitable)
    returnA -< output
  where algo = (fromParameters algoDesc) params

-- preparing --

castToNumericFeatures :: Pipeline DataTable NumericTable
castToNumericFeatures = proc table -> do
  let nums = numericColumns table
  let nats = naturalColumns table
  let noms = nominalColumns table
  let bins = binaryColumns table
  encodedNoms <- transform oneHotEncode -< noms
  uni <- inject -< featureUnion [binaryToNumeric encodedNoms, binaryToNumeric bins,
    naturalToNumeric nats]
  returnA -< nums

castToBinaryFeatures :: Pipeline DataTable BinaryTable
castToBinaryFeatures = proc table -> do
  noms <- inject -< asTable table :: Fallible NominalTable
  encodedNoms <- transform oneHotEncode -< noms
  returnA -< encodedNoms

castToNumericTarget :: DataColumn -> Fallible NumericColumn
castToNumericTarget dataCol =
  (asColumn dataCol :: Fallible NumericColumn) `orElse`
    do
      binary <- asColumn dataCol :: Fallible BinaryColumn
      return $ binaryToNumericColumn binary

castToBinaryTarget :: DataColumn -> Fallible BinaryColumn
castToBinaryTarget dataCol = binCol `orElse` nomColCasted
  where
    binCol = (asColumn dataCol :: Fallible BinaryColumn)
    nomColCasted = do
      nominal <- asColumn dataCol :: Fallible NominalColumn
      nominalToBinaryColumn nominal

numericPrepare :: Pipeline (DataTable, DataColumn) (NumericTable, NumericColumn)
numericPrepare = proc (x, y) -> do
  x' <- castToNumericFeatures -< x
  y' <- inject -< castToNumericTarget y
  returnA -< (x', y')

numericPrepareWithBinTarget :: Pipeline (DataTable, DataColumn) (NumericTable, BinaryColumn)
numericPrepareWithBinTarget = proc (x, y) -> do
  x' <- castToNumericFeatures -< x
  y' <- inject -< castToBinaryTarget y
  returnA -< (x', y')

binaryPrepare :: Pipeline (DataTable, DataColumn) (BinaryTable, BinaryColumn)
binaryPrepare = proc (x, y) -> do
  x' <- castToBinaryFeatures -< x
  y' <- inject -< castToBinaryTarget y
  returnA -< (x', y')

-- algorithms --

learningRateStr = "learning rate"
numberOfIterationsStr = "number of iterations"
lambdaStr = "lambda"
addInterceptStr = "add intercept"

smoothingValueStr = "smoothing value"

linRegAlgoDesc :: AlgorithmDesc LinearRegression Numeric Numeric
linRegAlgoDesc = AlgorithmDesc {
  name = "Linear regression"
  , desc = "Builds linear model with GD"
  , expectedFeatures = staticType
  , expectedTarget = staticType
  , defaultParams = LinReg.defaultLinearRegression

  , toParameters = \linReg -> Map.fromList [
      (learningRateStr, DoubleParam (LinReg.learningRate linReg) (Just (0, maxDouble)) 0.0000001)
      , (numberOfIterationsStr, IntParam (LinReg.numberOfIterations linReg) (Just (1, maxBound)))
      , (lambdaStr, DoubleParam (LinReg.lambda linReg) (Just (0, maxDouble)) 0.1)
      , (addInterceptStr, BoolParam (LinReg.addIntercept linReg))
    ]
  , fromParameters = \map -> let get k = map Map.! k in
      LinReg.LinearRegression {
        LinReg.learningRate = case get learningRateStr of DoubleParam x _ _  -> x
        , LinReg.numberOfIterations = case get numberOfIterationsStr of IntParam x _ -> x
        , LinReg.lambda = case get lambdaStr of DoubleParam x _ _ -> x
        , LinReg.addIntercept = case get addInterceptStr of BoolParam x -> x
      }

  , prepareData = numericPrepare
  , runAlgo = \linReg -> proc (x, y) -> do
      (model, mse, rmse) <- transform (fitAndEvaluate' linReg) -< (x, y)
      let output = "\n\n Model:" ++ show model ++
            "\n\n Mean squared error: " ++ show mse ++
            "\n\n Root mean squared error: " ++ show rmse
      returnA -< output
}

fitAndEvaluate' :: LinearRegression ->
  (NumericTable, NumericColumn) -> (NumericTable, NumericColumn) ->
  Fallible (LinReg.LinearRegressionModel, Double, Double)
fitAndEvaluate' linReg train (testX, testY) = do
  model <- fit linReg train
  mse <- evaluate model MeanSquaredError testX testY
  rmse <- evaluate model RootMeanSquaredError testX testY
  return (model, mse, rmse)

-- logistic regression

logRegAlgoDesc :: AlgorithmDesc LogisticRegression Numeric Binary
logRegAlgoDesc = AlgorithmDesc {
  name = "Logistic regression"
  , desc = "Builds linear model with GD"
  , expectedFeatures = staticType
  , expectedTarget = staticType
  , defaultParams = LogReg.defaultLogisticRegression

  , toParameters = \linReg -> Map.fromList [
      (learningRateStr, DoubleParam (LogReg.learningRate linReg) (Just (0, maxDouble)) 0.0000001)
      , (numberOfIterationsStr, IntParam (LogReg.numberOfIterations linReg) (Just (1, maxBound)))
      , (lambdaStr, DoubleParam (LogReg.lambda linReg) (Just (0, maxDouble)) 0.1)
      , (addInterceptStr, BoolParam (LogReg.addIntercept linReg))
    ]
  , fromParameters = \map -> let get k = map Map.! k in
      LogReg.LogisticRegression {
        LogReg.learningRate = case get learningRateStr of DoubleParam x _ _  -> x
        , LogReg.numberOfIterations = case get numberOfIterationsStr of IntParam x _ -> x
        , LogReg.lambda = case get lambdaStr of DoubleParam x _ _ -> x
        , LogReg.addIntercept = case get addInterceptStr of BoolParam x -> x
      }

  , prepareData = numericPrepareWithBinTarget
  , runAlgo = \logReg -> proc (x, y) -> do
      (model, acc, prec, recall) <- transform (fitAndEvaluateLogReg logReg) -< (x, y)
      let output = "\n\n Model:" ++ show model ++
            "\n\n Accuracy: " ++ show acc ++
            "\n\n Precision: " ++ show prec ++
            "\n\n Recall: " ++ show recall
      returnA -< output
}

fitAndEvaluateLogReg :: LogisticRegression ->
  (NumericTable, BinaryColumn) -> (NumericTable, BinaryColumn) ->
  Fallible (LogReg.LogisticRegressionModel, Double, Double, Double)
fitAndEvaluateLogReg logReg train (testX, testY) = do
  model <- fit logReg train
  acc <- evaluate model Accuracy testX testY
  prec <- evaluate model Precision testX testY
  recall <- evaluate model Recall testX testY
  return (model, acc, prec, recall)

-- naive bayes
naiveBayesAlgoDesc :: AlgorithmDesc NaiveBayes Binary Binary
naiveBayesAlgoDesc = AlgorithmDesc {
  name = "Naive Bayes"
  , desc = "Builds linear model with GD"
  , expectedFeatures = staticType
  , expectedTarget = staticType
  , defaultParams = defaultNaiveBayes

  , toParameters = \nb -> Map.fromList [
      (smoothingValueStr, DoubleParam (smoothingValue nb) (Just (0, maxDouble)) 0.001)
    ]
  , fromParameters = \map -> let get k = map Map.! k in
      NaiveBayes {
        smoothingValue = case get smoothingValueStr of DoubleParam x _ _  -> x
      }

  , prepareData = binaryPrepare
  , runAlgo = \logReg -> proc (x, y) -> do
      (model, acc, prec, recall) <- transform (fitAndEvaluateNaiveBayes logReg) -< (x, y)
      let output = "\n\n Model:" ++ show model ++
            "\n\n Accuracy: " ++ show acc ++
            "\n\n Precision: " ++ show prec ++
            "\n\n Recall: " ++ show recall
      returnA -< output
}

fitAndEvaluateNaiveBayes :: NaiveBayes ->
  (BinaryTable, BinaryColumn) -> (BinaryTable, BinaryColumn) ->
  Fallible (BernoulliModel, Double, Double, Double)
fitAndEvaluateNaiveBayes nb train (testX, testY) = do
  model <- fit nb train
  acc <- evaluate model Accuracy testX testY
  prec <- evaluate model Precision testX testY
  recall <- evaluate model Recall testX testY
  return (model, acc, prec, recall)
