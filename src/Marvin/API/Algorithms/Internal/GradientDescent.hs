{-|
Module      : Marvin.API.Algorithms.Internal.GradientDescent
Description : Optimization algorithm.
-}
module Marvin.API.Algorithms.Internal.GradientDescent where

import Marvin.API.Table.Internal as Table
import Marvin.API.Fallible

import Numeric.LinearAlgebra.HMatrix
import Numeric.LinearAlgebra.Data as LA

import qualified Data.Vector as Vec
import qualified Data.Vector.Unboxed as UVec

import Control.Arrow
import Control.Monad.Except
import Data.List (intercalate)

-- | Resulting model
data LinearModel = LM {
  featureColumnNames :: [Fallible ColumnName]
  , targetVariableName :: Fallible ColumnName
  , coefs :: LA.Vector Double
  , intercept_ :: Double
} deriving Eq

instance Show LinearModel where
  show model = intercalate " +\n" $ map ((++) "\t") $ show (intercept_ model) : columnStrings
    where
      columnStrings = map (\(idx, (coef, fallibleName)) ->
        let name = case fallibleName of
              Right n -> n
              Left _ -> "column #" ++ show idx
        in show coef ++ " * " ++ name) $
        zip [0..] $ zip (LA.toList (coefs model)) (featureColumnNames model)

fit' :: GradientDescent -> (NumericTable, NumericColumn) -> LinearModel
fit' gradDesc (nt, nc) = LM {
    featureColumnNames = columnNames nt
    , targetVariableName = columnName nc
    , intercept_ = intercept'
    , coefs = theta'
  }
  where
    featureMtx = tableToMatrix nt
    targetVar = columnToVector nc
    modelParams = gradientDescent gradDesc featureMtx targetVar
    theta = modelParams
    (intercept', theta') = if (addIntercept gradDesc)
      then (theta ! 0, LA.subVector 1 (LA.size theta - 1) theta)
      else (0, theta)

predict' :: LinearModel -> NumericTable -> Fallible NumericColumn
predict' model test = do
      dimCheck
      return $ unsafePredict model test
    where
      modelDim = LA.size $ coefs model
      testDim = numberOfColumns test
      dimCheck = if modelDim == testDim
          then return ()
          else throwError $
            RowLengthMismatch $
              "When attempting to make predictions based on a linear model. " ++
              "coefficients: " ++ (show modelDim) ++
              ", columns: " ++ (show testDim) ++ "."

-- | Predicts without checks.
unsafePredict :: LinearModel -> NumericTable -> NumericColumn
unsafePredict model nt = pred
  where
    pred = unsafeFromVector $ fromLinAlgVector $ batchPredict theta c x
    x = tableToMatrix nt
    theta = coefs model
    c = intercept_ model

-- | Predicts with hmatrix types.
batchPredict :: LA.Vector R -> R -> FeatureMatrix -> TargetVariable
batchPredict theta intercept x = cmap (+ intercept) (x #> theta)

-- * Type aliases
type TargetVariable   = Vector R
type FeatureVector    = Vector R

type FeatureMatrix    = Matrix R
type ModelParameters  = Vector R

type CostFunction = FeatureMatrix -> TargetVariable -> ModelParameters -> (R, Vector R)
type Regularization = R -> ModelParameters -> (R, Vector R)

-- * Parameters
data GradientDescent = GradientDescent {
  learningRate :: Double
  , addIntercept :: Bool
  , lambda :: Double
  , cost :: CostFunction
  , initModelParams :: FeatureMatrix -> TargetVariable -> ModelParameters
  , numIter :: Int
}

defaultGradientDescent = GradientDescent {
  learningRate = 0.1
  , initModelParams = \x y -> cols x |> [1,1..]
  , numIter = 10
  , lambda = 0.1
  , addIntercept = True
  , cost = linearRegressionCost
}
-- * Cost functions

logisticRegressionCost :: FeatureMatrix -> TargetVariable -> ModelParameters
                                                     -> (R, Vector R)
logisticRegressionCost x@features y@targetVariable theta@modelParameters =
  (cost, gradient)
  where
    cost            = (1 / m) * sumElements (-y * log h - (1-y) * log(1-h))
    gradient        = scale (1 / m) $ tr x #> error
    m               = fromIntegral (rows x) :: R
    error           = h - y
    h@hypothesis    = cmap sigmoid $ x #> theta

linearRegressionCost :: FeatureMatrix -> TargetVariable -> ModelParameters -> (R, Vector R)
linearRegressionCost x@features y@targetVariable theta@modelParameters =
  (cost, gradient)
  where
    cost            = (1 / (2 * m)) * sumElements (error ^ 2)
    gradient        = scale (1 / m) $ tr x #> error
    m               = fromIntegral (rows x) :: R
    error           = h - y
    h@hypothesis    = x #> theta

addRegularization :: R -> CostFunction -> Regularization -> CostFunction
addRegularization lambda costFunction regularization x y theta =
  (cost + regCost, gradient + regGradient)
  where
    (cost, gradient) = costFunction x y theta
    (regCost, regGradient) = regularization lambda theta

l2 :: R -> ModelParameters -> (R, Vector R)
l2 lambda@regularizationCoefficient theta@modelParameters =
  (cost, gradient)
  where
    cost = lambda * (norm_2 modelParameters)^2
    gradient = scale (2 * lambda) modelParameters

-- * Algorithm

gradientDescent :: GradientDescent -> FeatureMatrix -> TargetVariable ->
  ModelParameters
gradientDescent params featureMtx targetVariable =
  runIteration (numIter params) modelUpdater initModel
    where
      features = if addIntercept params
        then addInterceptFeature featureMtx
        else featureMtx
      lambda' = lambda params
      cost' = cost params
      costFunction = if lambda' /= 0
        then addRegularization lambda' cost' l2
        else cost'
      alpha                       = learningRate params
      initModel                   = initModelParams params features targetVariable
      gradient model              = snd $ costFunction features targetVariable model
      modelUpdater theta          = theta - scale alpha (gradient theta)

-- * Helpers

addInterceptFeature :: FeatureMatrix -> FeatureMatrix
addInterceptFeature features = LA.fromColumns $ constantFeature:toColumns x
  where
    x                   = features
    (m,_)               = size x
    constantFeature     = m |> repeat 1

sigmoid :: R -> R
sigmoid z = 1 / (1 + exp(-z))

runIteration :: Int -> (a -> a) -> a -> a
runIteration n f init = iterate f init !! n

-- * Conversions

columnToVector :: NumericColumn -> LA.Vector R
columnToVector = LA.fromList . UVec.toList . values

tableToMatrix :: NumericTable -> LA.Matrix R
tableToMatrix =
  columnsVec >>>
  Vec.toList >>>
  map columnToVector >>>
  LA.fromColumns

fromLinAlgVector :: LA.Vector R -> UVec.Vector R
fromLinAlgVector = LA.toList >>> UVec.fromList

toLinAlgVector :: Vec.Vector R -> LA.Vector R
toLinAlgVector = Vec.toList >>> LA.fromList