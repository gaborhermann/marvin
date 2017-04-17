module MarvinSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = describe "marvin" $ do
        it "returns the unit value" $
           True
