module Marvin.API.Algorithms.LinearRegressionSpec where

import Test.Hspec

import Marvin.API
import Marvin.API.Algorithms.LinearRegression
import Marvin.Test.TestUtils

spec :: Spec
spec =
  describe "linear regression" $ do
    it "can be fit and evaluated" $
      trained `shouldSatisfy` \res -> case res of
        Right (x,y,x',y',model,pred,mse) ->
          (mse `equals` 0.5872961757090328) ((+-) 1e-10) &&
          (y `equals` y') ((+-) 1e-10) &&
          (x `equals` x') ((+-) 1e-10) &&
          (coefficients model `equals` tail expectedTheta) ((+-) 1e-10) &&
          (intercept model `equals` head expectedTheta) ((+-) 1e-10)
        Left _ -> False
    it "gives error on empty table" $
      fitEmpty `shouldSatisfy` \res -> case res of
        (Left (ColumnSizeMismatch _)) -> True
        _ -> False
    it "gives error on empty table at prediction" $
      predictEmpty `shouldSatisfy` \res -> case res of
        (Left (RowLengthMismatch _)) -> True
        _ -> False
  where
    fitEmpty = do
      y <- trainY
      fit linRegParams (emptyTable :: NumericTable, y)
    predictEmpty = do
      model <- linRegModel
      predict model (emptyTable :: NumericTable)
    linRegModel = fmap (\(_,_,_,_,m,_,_) -> m) trained
    trained = do
      wholeTable <- trainData
      let targetCol = byIndex 11
      (x', y') <- selectTargetVariable targetCol wholeTable
      x <- trainX
      y <- trainY
      model <- fit linRegParams (x', y')
      pred <- predict model x
      mse <- evaluate model MeanSquaredError x y
      return (x,y,x',y',model,pred,mse)

    linRegParams = LinearRegression {
      numberOfIterations = 30000,
      learningRate = 0.005,
      lambda = 0,
      addIntercept = True
    }

    trainX :: Fallible NumericTable
    trainX = fromRows xData

    trainY :: Fallible NumericColumn
    trainY = fromList yData

    trainData :: Fallible NumericTable
    trainData = fromRows trainMatrix

    xData = map init trainMatrix
    yData = map last trainMatrix

    expectedTheta =
      [13.781001191549892,
      3.900308511353248e-2,
      -8.360245732382168e-2,
      2.8699265280611834e-2,
      2.7797762929208453e-2,
      0.13913675368346703,
      3.900308511353248e-2,
      0.11340373429004338,
      0.1166082670688832,
      4.446542254295697e-2,
      0.14211000816593214,
      4.446542254295697e-2]
