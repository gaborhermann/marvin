{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Arrows #-}
module Marvin.Examples.SpamClassification where

import System.Directory
import Control.Arrow
import Data.List
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe

import Marvin.API
import Marvin.API.Algorithms.NaiveBayes

spamPipe :: Pipeline [Text] BinaryTable
spamPipe = proc docs -> do
  let docWords = fmap docToWords docs
  flaggedWords <- transform wordToVec -< docWords
  table <- inject -< fromRows flaggedWords
  returnA -< table

type SpamFolder = String
type HamFolder = String

spam :: SpamFolder -> HamFolder -> IO (Fallible (PipelineModel BernoulliModel [Text]))
spam spamDir nonSpamDir = do
  spams <- dirToDocs spamDir
  nonSpams <- dirToDocs nonSpamDir
  let emails =
        (fmap (\spam -> (spam, True)) spams) ++
        (fmap (\nonSpam -> (nonSpam, False)) nonSpams)
  let (texts, flags) = unzip emails
  let naiveBayes = NaiveBayes { smoothingValue = 0.5 }
  let model = do
        isSpamColumn <- fromList flags
        bernoulliModel <- fit naiveBayes (spamPipe, texts, isSpamColumn)
        return bernoulliModel
  return model

docToWords :: Text -> [Text]
docToWords doc = map T.toLower $
    filter (\w -> T.length w > 2) $ T.words doc

type Dictionary = Set Text

createDictionary :: [[Text]] -> Dictionary
createDictionary = concat >>> Set.fromList

toWordVec :: Dictionary -> [Text] -> [Bool]
toWordVec dict doc = map (\i -> Set.member i indices) [0..(n-1)]
  where
    indices = Set.fromList $ catMaybes $ map (\w -> Set.lookupIndex w dict) doc
    n = Set.size dict

dirToDocs :: FilePath -> IO [Text]
dirToDocs dirPath = do
  paths <- getDirectoryContents dirPath
  mapM TIO.readFile $
    map (\x -> dirPath ++ "/" ++ x) $
    filter (\x -> x /= ".." && x /= ".") paths

wordToVec :: [[Text]] -> [[Text]] -> Fallible [[Bool]]
wordToVec train = return . map (toWordVec dict)
  where
    dict = createDictionary train
