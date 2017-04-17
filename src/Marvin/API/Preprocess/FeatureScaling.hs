{-|
Module      : Marvin.API.Preprocess.FeatureScaling
Description : Scaling columns to a normalized form.
-}
module Marvin.API.Preprocess.FeatureScaling where

import Marvin.API.Table.DataType
import Marvin.API.Fallible
import Marvin.API.Meta.Model
import Marvin.API.Table.Internal
import Data.Vector.Unboxed as UVec
import Data.Vector as Vec

import Control.Arrow

-- | Scales the elements of all columns in the table linearly,
-- so that every element is between 0 and 1.
minMaxScale :: NumericTable -> (NumericTable -> Fallible NumericTable)
minMaxScale = transformEveryColumn "While scaling by min-max." $
  \x -> return . (minMaxScaleColumn x)

-- | Scales the elements of a column linearly,
-- so that every element is between 0 and 1.
minMaxScaleColumn :: NumericColumn -> (NumericColumn -> NumericColumn)
minMaxScaleColumn col = applyLinFuncToCol linFunc
  where
    linFunc = linearFunction $ if (max - min > 0)
      then (-min, 1 / (max - min))
      else (0, 1)
    vals = values col
    min = UVec.minimum vals
    max = UVec.maximum vals

-- | Scales the elements of all columns in the table linearly,
-- so that their mean is 0 and standard deviation is 1.
standardize :: NumericTable -> (NumericTable -> Fallible NumericTable)
standardize = transformEveryColumn "While standardizing table." $
  \x -> return . (standardizeColumn x)

-- | Scales the elements of a column linearly,
-- so that their mean is 0 and standard deviation is 1.
standardizeColumn :: NumericColumn -> (NumericColumn -> NumericColumn)
standardizeColumn col = applyLinFuncToCol $ vectorStandardizer $ values col

-- (standardized, added, then scaled)
-- in other way: (standardized, avg, sigma)
vectorStandardizer :: UVec.Vector Double -> LinearFunction
vectorStandardizer v = if stdDeviation /= 0
  then linearFunction (-avg, 1 / stdDeviation)
  else linearFunction (0, 1)
    where
      n                 = UVec.length v
      avg               = UVec.sum v / fromIntegral n
      stdDeviation      = sqrt $ (UVec.sum . UVec.map (\x -> (x - avg)^2)) v / fromIntegral n

-- | Type used for linear scaling. When applying this to a given value,
-- the first one parameter is added to the value, and the result is scaled by the second.
data LinearFunction = LinearFunction (Double, Double) deriving (Show)


-- | Creates a 'LinearFunction'.
-- The first will be added to the value, and the result is scaled by the second.
linearFunction :: (Double, Double) -> LinearFunction
linearFunction = LinearFunction

-- | Applies linear function to single value.
applyLinFunc :: LinearFunction -> Double -> Double
applyLinFunc (LinearFunction (plus, coef)) = (* coef) . (+ plus)

-- | Applies linear function to a vector.
applyLinFuncToVec :: LinearFunction -> UVec.Vector Double -> UVec.Vector Double
applyLinFuncToVec linFunc v = UVec.map (applyLinFunc linFunc) v

-- | Applies linear function to a column.
applyLinFuncToCol :: LinearFunction -> NumericColumn -> NumericColumn
applyLinFuncToCol linFunc col = col { values = applyLinFuncToVec linFunc (values col) }
