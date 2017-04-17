{-|
Module      : Marvin.API.Table.TrainTestSplit
Description : Splitting tables to training and test data.
-}
module Marvin.API.Table.TrainTestSplit where

import System.Random

import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Data.Vector.Unboxed as UVec

import Control.Arrow

import Marvin.API.Fallible
import Marvin.API.Table.Internal

import Control.Monad

-- | Splits a table randomly to a pair of (training data, test data)
-- by a given ratio of training data from all data.
trainTestSplit :: TableClass t =>
  Double -- ^ training:all ratio
  -> t
  -> IO (Fallible (t, t))
trainTestSplit trainPercentage table = do
  g <- newStdGen
  return $ trainTestSplitRandom g trainPercentage table

trainTestSplitRandom :: (TableClass t, RandomGen g) =>
  g -> Double -> t -> Fallible (t, t)
trainTestSplitRandom gen trainPercentage = splitTableByFlags flags
  where
    flags = randomFlags gen trainPercentage

splitTableByFlags :: TableClass t => [Bool] -> t -> Fallible (t, t)
splitTableByFlags flags table = do
  zippedCols <- traverse (splitByFlags flags) cols
  let (cols1, cols2) = Vec.unzip zippedCols
  return (unsafeFromCols cols1, unsafeFromCols cols2)
  where
    cols = columnsVec table

randomFlags :: RandomGen g => g -> Double -> [Bool]
randomFlags gen trainPercentage = map (< trainPercentage) (randomRs (0, 1) gen :: [Double])
