{-|
Module      : Marvin.API.Meta.Pipeline
Description : Chained transformations of training and test data.
-}
module Marvin.API.Meta.Pipeline (
  Pipeline
  , transform
  , inject
  , fitThenRun
) where

import Prelude hiding (id, (.))
import Control.Arrow
import Control.Monad
import Control.Category

import Marvin.API.Fallible

-- * Pipeline

-- Note: Although 'Trainable' could express 'Simple', then the identity property of category
-- would not be satisfied because of Fallible side-effect.
-- In the composition of 'Trainable's (f x x) might be Left Error, so even if g does not use f x x
-- it's going to return Error.
-- Furthermore, using 'Simple' might lead to more efficient computation as there's no need
-- to compute (f x x).
data MonadicPipeline m a b = Trainable (a -> a -> m b) | Simple (a -> m b)

-- | A machine learning pipe representing chained transformations from type 'a' to type 'b'.
-- The underlying type is (a -> a -> Fallible b). The transformation is fitted by the first argument
-- and the second argument is transformed. This is analogous to the fit and transform methods
-- in scikit-learn.
type Pipeline a b = MonadicPipeline Fallible a b

instance (Monad m) => Category (MonadicPipeline m) where
  id = Simple return
  -- f :: a -> a -> m b
  -- g :: b -> b -> m c
  (Trainable g) . (Trainable f) = Trainable $ \x x' -> do
      let f' = f x
      fx' <- f' x'
      fx <- f' x
      g fx fx'
  (Trainable g) . (Simple f') = Trainable $ \x x' -> do
      fx <- f' x
      fx' <- f' x'
      g fx fx'
  (Simple g') . (Trainable f) = Trainable $ \x x' -> do
      fx' <- f x x'
      g' fx'
  (Simple g') . (Simple f') = Simple $ \x -> do
      f'x <- f' x
      g' f'x

instance (Monad m) => Arrow (MonadicPipeline m) where
  arr f = Simple $ return . f
  first (Trainable f) = Trainable $ \(x,y) (x',y') -> do
    fxx' <- f x x'
    return (fxx', y')
  first (Simple f) = Simple $ \(x,y) -> do
    fx <- f x
    return (fx, y)

-- | Primary constructor of 'Pipeline'.
transform :: (a -> a -> Fallible b) -> Pipeline a b
transform = Trainable

-- | A pipeline that passes a 'Fallible' value forward and does not modify it.
inject :: Pipeline (Fallible a) a
inject = Simple id

-- | Evaluates a pipeline.
fitThenRun ::
  Pipeline a b -- ^ Pipeline.
  -> a -- ^ Training data.
  -> (a -> Fallible b) -- ^ Transformation learned by the training data.
fitThenRun (Trainable f) = f
fitThenRun (Simple f') = const f'

