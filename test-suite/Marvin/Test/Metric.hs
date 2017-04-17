{-# LANGUAGE FlexibleInstances #-}
module Marvin.Test.Metric where

import Marvin.API.Table.Internal
import Numeric.LinearAlgebra.HMatrix
import Numeric.LinearAlgebra.Data
import qualified Data.Vector as Vec
import qualified Data.Vector.Unboxed as UVec
import Data.Array

class Metric a where
    dist :: a -> a -> Double

instance Metric R where
    dist a b = abs $ a - b

instance Metric (Vector R) where
    dist a b = norm_Inf $ a - b

instance Metric (Matrix R) where
    dist a b = norm_Inf $ a - b

instance Metric (Vec.Vector R) where
    dist a b = Vec.foldl1 max $ Vec.zipWith (\x y -> abs (x - y)) a b

instance Metric (UVec.Vector R) where
    dist a b = UVec.foldl1 max $ UVec.zipWith (\x y -> abs (x - y)) a b

instance Metric NumericColumn where
    dist a b = dist (values a) (values b)

instance Metric NumericTable where
    dist a b = Vec.maximum $ Vec.zipWith dist (columnsVec a) (columnsVec b)

instance Metric [Double] where
    dist a b = maximum $ zipWith dist a b

instance Metric [[Double]] where
    dist a b = maximum $ zipWith dist a b

-- infinite norm
instance (Ix i, Metric a) => Metric (Array i a) where
    dist a b = maximum $ zipWith dist (elems a) (elems b)
