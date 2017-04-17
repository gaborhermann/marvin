{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Marvin.API.Algorithms.KMeans (
  KMeans (..)

  , CentroidModel
  , distancesFromNearestCentroids
  , centroids
) where

import Control.Monad.Reader

import Data.Foldable
import Data.Array.IArray
import Data.Ord
import Control.Arrow
import Control.Monad.Except

import Marvin.API.Table.DataType
import Marvin.API.Fallible
import Marvin.API.Meta.Model
import Marvin.API.Table.DataType
import Marvin.API.Fallible
import Marvin.API.Table.Internal

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as UVec
import qualified Data.Vector as Vec

import qualified Numeric.LinearAlgebra as LA

-- * Parameters

-- | Parameters.
data KMeans = KMeans {
  k :: Int -- ^ Number of clusters (centroids).
  , numberOfIterations :: Int -- ^ Number of iterations.
}

-- | Default parameters.
defaultKMeans = KMeans {
  k = 5
  , numberOfIterations = 10
}

-- * Model

-- | Resulting model.
data CentroidModel = CentroidModel {
  centroids_ :: Array Label Centroid
  , dimension :: Int
} deriving Show

-- | Retrieves a list of the centroids.
centroids :: CentroidModel -> [[Double]]
centroids model = fmap LA.toList $ fmap snd $ assocs $ centroids_ model

-- | Retrieves the distances from the nearest centroids for some points
distancesFromNearestCentroids :: CentroidModel -> NumericTable -> Fallible NumericColumn
distancesFromNearestCentroids model tab = do
  if dimension model /= numberOfColumns tab
    then throwError $ RowLengthMismatch $ "While trying the calculate nearest distances " ++
      "with CentroidModel."
    else return ()
  let ps = tableToRows tab
  let distances = fmap (distanceFromNearest model) ps
  fromList $ Vec.toList $ distances

-- | Type aliases.
type Point = LA.Vector Double
type Centroid = LA.Vector Double
type Label = Int

-- | Distance from nearest centroid.
distanceFromNearest :: CentroidModel -> Point -> Double
distanceFromNearest model p = minimum $ fmap (dist p) cs
  where
    cs = fmap snd $ assocs $ centroids_ model


instance Estimator KMeans NumericTable where
  type ResultingModel KMeans NumericTable = CentroidModel
  fit = fit'

fit' :: KMeans -> NumericTable -> Fallible CentroidModel
fit' kMeans table = do
  ensurePositive
    (InvalidAlgorithmParameter "KMeans" "numberOfIterations must be positive") numIter
  ensurePositive
    (InvalidAlgorithmParameter "KMeans" "number of centroids (k) must be positive") numCentroids
  ensureNonEmptyTable "While fitting KMeans." table
  return $ CentroidModel {
    centroids_ = centroidArray
    , dimension = numberOfColumns table
  }
  where
    centroidArray = fitKMeans params numIter
    numIter = numberOfIterations kMeans
    numCentroids = k kMeans
    pts = tableToRows table
    params = KMeansParams {
      numOfCents = numCentroids
      , points = Vec.toList pts
    }

instance Predictor CentroidModel where
  type Testing CentroidModel = NumericTable
  type Prediction CentroidModel = NaturalColumn
  predict model numericTable = if numberOfColumns numericTable /= dimension model
      then throwError $ RowLengthMismatch "When predicting with a CentroidModel."
      else fromList $ Vec.toList labels
    where
      pts = tableToRows numericTable
      labels = fmap (nearestLabel (centroids_ model)) pts

tableToRows =
  Vec.map (LA.fromList . UVec.toList) . transposeVectorVectorU . Vec.map values . columnsVec

data InitStrategy = TakeFirstK
data KMeansParams = KMeansParams {
  points :: [Point]
  , numOfCents :: Int
}

dimensions :: KMeansParams -> Int
dimensions params = LA.size $ head $ points params

initCents :: Reader KMeansParams (Array Label Centroid)
initCents = do
  ps <- asks points
  k <- asks numOfCents
  return $ listArray (1,k) (take k (ps))

dist :: LA.Vector Double -> LA.Vector Double -> Double
dist xs ys = LA.norm_2 $ xs - ys

nearestLabels :: Array Label Centroid -> Reader KMeansParams [(Label, Point)]
nearestLabels cs = do
  ps <- asks points
  return $
    fmap (\p -> (nearestLabel cs p, p)) ps

nearestLabel :: Array Label Centroid -> Point -> Label
nearestLabel cs p = fst $ minimumBy (comparing (dist p . snd)) (assocs cs)

mean :: [Vector Double] -> Vector Double
mean xs = UVec.map (\x -> x / n) $ sumVecs xs
  where
    n = fromIntegral (length xs)
    sumVecs = foldr1 addVecs
    addVecs x y = UVec.zipWith (+) x y

accumArrWithLabs :: Ix i => Array i (Point, Int) -> [(i, Point)] -> Array i (Point, Int)
accumArrWithLabs = accum meanAccum

meanAccum :: (Point, Int) -> Point -> (Point, Int)
meanAccum (sum, cnt) p = (sum + p, cnt + 1)

nextCentroids :: Array Label Centroid -> Reader KMeansParams (Array Label Centroid)
nextCentroids cs = do
  ps <- asks points
  k <- asks numOfCents
  n <- asks dimensions
  let initArr = listArray (1,k) (repeat (n LA.|> [0,0..], 0))
  labeledPoints <- nearestLabels cs
  let means = accum meanAccum initArr labeledPoints
  return $ amap (\(sum, cnt) -> LA.scale (1 / fromIntegral cnt) sum) means

fitKMeans :: KMeansParams -> Int
  -> Array Label Centroid
fitKMeans params numIter = runReader readerIter params
  where
    readerIter = do
      init <- initCents
      runIterationM numIter nextCentroids init

runIterationM :: Monad m => Int -> (a -> m a) -> a -> m a
runIterationM n f init = iterate (>>= f) (return init) !! n
