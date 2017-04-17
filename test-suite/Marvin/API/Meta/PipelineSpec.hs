{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Marvin.API.Meta.PipelineSpec where

import Prelude hiding (id, (.))

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function

import Marvin.API as Marvin
import Marvin.Test.TestUtils as Marvin
import Control.Arrow
import Control.Category

spec :: Spec
spec =
  describe "pipeline" $ do
    it "transform works" $
      (fitThenRun p1) "a" "b" `shouldBe` (Right "a?b")
    it "transform composes" $
      (fitThenRun (p1 >>> p2)) "a" "b" `shouldBe` (Right "a?a!a?b")
    it "inject works" $
      (fitThenRun inject) (Right "useless") (Right "b") `shouldBe` (Right "b")
    it "category assoc" $
      property $ \f g h x x' ->
        let types = [f,g,h] :: [Pipeline String String] in
          (fitThenRun (f . (g . h))) x x'
            `shouldBe`
          (fitThenRun ((f . g) . h)) x x'
    it "category id 1" $
      property $ \f x x' ->
        let types = f :: Pipeline String String in
          (fitThenRun (f . id)) x x'
            `shouldBe`
          (fitThenRun (id . f)) x x'
    it "category id 2" $
      property $ \f x x' ->
        let types = f :: Pipeline String String in
          (fitThenRun (f . id)) x x'
            `shouldBe`
          (fitThenRun f) x x'
    it "arrow property 1" $
      property $ \x x' ->
        (fitThenRun (arr id)) x x' `shouldBe` (fitThenRun (id :: Pipeline String String)) x x'
    it "arrow property 2" $
      property $ \(Fun _ fun1) (Fun _ fun2) x x' ->
        let
          f = fun1 :: Int -> Int
          g = fun2 :: Int -> String
        in
          (fitThenRun (arr (f >>> g))) x x'
            `shouldBe`
          (fitThenRun (arr f >>> arr g)) x x'
    it "arrow property 3" $
      property $ \(Fun _ fun) v1 x' ->
        let
          f = fun :: Int -> String
          x = v1 :: (Int, Double)
        in
          (fitThenRun (first (arr f))) x x'
            `shouldBe`
          (fitThenRun (arr (first f))) x x'
    it "arrow property 4" $
      property $ \p1 p2 v1 x' ->
        let
          f = p1 :: Pipeline Int Int
          g = p2 :: Pipeline Int String
          x = v1 :: (Int, Double)
        in
          (fitThenRun (first (f >>> g))) x x'
            `shouldBe`
          (fitThenRun (first f >>> first g)) x x'
    it "arrow property 5" $
      property $ \p1 v1 x' ->
        let
          f = p1 :: Pipeline Int String
          x = v1 :: (Int, Double)
        in
          (fitThenRun (first f >>> arr fst)) x x'
            `shouldBe`
          (fitThenRun (arr fst >>> f)) x x'
    it "arrow property 6" $
      property $ \p1 (Fun _ fun1) v1 x' ->
        let
          f = p1 :: Pipeline Int Int
          g = fun1 :: Int -> String
          x = v1 :: (Int, Int)
        in
          (fitThenRun (first f >>> arr (id *** g))) x x'
            `shouldBe`
          (fitThenRun (arr (id *** g) >>> first f)) x x'
    it "arrow property 7" $
      property $ \p1 v1 x' ->
        let
          f = p1 :: Pipeline Int String
          x = v1 :: ((Int, Int), Double)
        in
          (fitThenRun (first (first f) >>> arr assoc)) x x'
            `shouldBe`
          (fitThenRun (arr assoc >>> first f)) x x'

p1 :: Pipeline String String
p1 = transform (\a b -> return (a ++ "?" ++ b))

p2 :: Pipeline String String
p2 = transform (\a b -> return (a ++ "!" ++ b))

instance (Function a, CoArbitrary a, Arbitrary a, Arbitrary b) => Arbitrary (Pipeline a b) where
  arbitrary = do
    (Fun _ f) <- arbitrary
    return $ transform f

instance Show (Pipeline a b) where
  show _ = "<pipe>"

xxx = do
  Left $ Marvin.Failure "a"
  Left $ Marvin.Failure "b"
  return 5

assoc ((a,b),c) = (a,(b,c))
