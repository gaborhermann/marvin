{-# LANGUAGE Arrows #-}
module Marvin.Examples.NetworkIntrusionDetection where

import Marvin.API
import Marvin.API.Algorithms.KMeans

import Data.List.Split

import Control.Arrow

anomalyExamples path = do
  Right riskOfAnomaly <- mkAnomalyDetector path
  let seemsNormal = "0,tcp,http,SF,1,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,2,"
        ++ "0.00,0.00,0.00,0.00,1.00,0.00,1.00,1,69,1.00,0.00,1.00,0.04,0.00,0.00,0.00,0.00,normal."
  let tooManyConnections = "0,tcp,http,SF,214,5301,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,2,"
        ++ "0.00,0.00,0.00,0.00,1.00,0.00,1.00,1,69,1.00,0.00,1.00,0.04,0.00,0.00,0.00,0.00,normal."
  print $ "normal: " ++ show (riskOfAnomaly seemsNormal)
  print $ "strange: " ++ show (riskOfAnomaly tooManyConnections)

mkAnomalyDetector :: FilePath -> IO (Fallible (String -> Fallible Double))
mkAnomalyDetector path = do
  csv <- fromCsv ',' NoHeader path
  return $ do
      let kmeans = KMeans { k = 10, numberOfIterations = 10 }
      raw <- csv
      model <- fit kmeans (preproc, raw)
      let distances = askModel model distancesFromNearestCentroids
      return $ distanceByOne distances

distanceByOne :: (RawTable -> Fallible NumericColumn) -> String -> Fallible Double
distanceByOne batchDistance connection = do
  rawTable <- fromRowsToRaw [splitOn "," connection]
  distance <-  batchDistance rawTable
  return $ head $ toList distance

preproc :: Pipeline RawTable NumericTable
preproc = proc raw -> do
  rawWithoutLabel <- inject -< removeColumn lastColumn raw
  let tab = smartParseTable rawWithoutLabel
  let noms = nominalColumns tab
  let nums = numericColumns tab
  let bins = binaryToNumeric $ binaryColumns tab
  let nats = naturalToNumeric $ naturalColumns tab
  encoded <- transform oneHotEncode -< noms
  allNums <- inject -< featureUnion [nums, bins, nats]
  normalized <- transform standardize -< allNums
  all <- inject -< featureUnion [normalized, binaryToNumeric encoded]
  returnA -< all

