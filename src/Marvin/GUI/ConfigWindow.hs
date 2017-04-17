{-# LANGUAGE ExistentialQuantification #-}
module Marvin.GUI.ConfigWindow where

import Marvin.API
import Marvin.GUI.AlgorithmDescriptor

import Graphics.UI.Gtk as Gtk

import Control.Monad.Trans.State
import Control.Concurrent.MVar

import Data.Map as Map

type ParameterState a = StateT Parameters IO a

paramsToConfigWindow :: Parameters -> IO Parameters
paramsToConfigWindow params = do
  dialog <- dialogNew
  windowSetTitle dialog "configure algorithm"
  windowSetModal dialog True
  windowSetDeletable dialog False
  -- button
  dialogAddButton dialog "OK" ResponseOk

  -- entries
  entries <- Map.traverseWithKey paramToHBox params

  vbox <- dialogGetUpper dialog
  traverse (\(entryBox, _) -> boxPackStart vbox entryBox PackNatural 0) entries

  onDestroy dialog $ dialogResponse dialog ResponseDeleteEvent
  widgetShowAll dialog

  response <- dialogRun dialog
  widgetDestroy dialog

  traverse (\(_, inputBox) -> retrieveParam inputBox) entries

paramToHBox :: String -> Parameter -> IO (HBox, InputBox)
paramToHBox name param = do
  hbox <- hBoxNew False 0
  label <- labelNew $ Just name

  inputBox <- paramToInputBox param

  boxPackStart hbox label PackNatural 0
  boxPackStart hbox (inputBoxWidget inputBox) PackNatural 0

  return (hbox, inputBox)

data InputBox = InputBox {
  inputBoxWidget :: Widget
  , retrieveParam :: IO Parameter
}

paramToInputBox :: Parameter -> IO InputBox
paramToInputBox (BoolParam value) = inputBoxBool value
paramToInputBox (IntParam value bounds) = inputBoxInt value bounds
paramToInputBox (DoubleParam value bounds stepSize) = inputBoxDouble value bounds stepSize

inputBoxBool :: Bool -> IO InputBox
inputBoxBool initial = do
  checkButton <- checkButtonNew
  toggleButtonSetActive checkButton initial
  return InputBox {
    inputBoxWidget = castToWidget checkButton
    , retrieveParam = do
        isActive <- toggleButtonGetActive checkButton
        return $ BoolParam isActive
    }

inputBoxDouble :: Double -> Maybe (Double, Double) -> Double -> IO InputBox
inputBoxDouble initial bounds stepSize = do
  let (lowest, highest) = case bounds of
        Just (l, h) -> (l, h)
        Nothing -> (fromIntegral (minBound :: Int), fromIntegral (maxBound :: Int))
  spinButton <- spinButtonNewWithRange lowest highest stepSize
  spinButtonSetValue spinButton initial
  return InputBox {
    inputBoxWidget = castToWidget spinButton
    , retrieveParam = do
        value <- spinButtonGetValue spinButton
        return $ DoubleParam value bounds stepSize
  }

inputBoxInt :: Int -> Maybe (Int, Int) -> IO InputBox
inputBoxInt initial bounds = do
  let (lowest, highest) = case bounds of
        Just (l, h) -> (l, h)
        Nothing -> (minBound, maxBound)
  spinButton <- spinButtonNewWithRange (fromIntegral lowest) (fromIntegral highest) 1
  spinButtonSetValue spinButton (fromIntegral initial)
  spinButtonSetDigits spinButton 0
  return InputBox {
    inputBoxWidget = castToWidget spinButton
    , retrieveParam = do
          value <- spinButtonGetValue spinButton
          return $ IntParam (round value) bounds
  }
