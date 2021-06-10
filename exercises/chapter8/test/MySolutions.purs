module Test.MySolutions where

import Data.Maybe
import Prelude

import Control.Apply (lift2)
import Control.Monad.ST (ST, for, run)
import Control.Monad.ST.Ref (modify, new, read, write)
import Control.Plus (empty)
import Data.Array (head, tail, sort, nub)
import Data.Foldable (foldM, sum)
import Data.Int (toNumber)
import Data.List (List(..), (:), filter, reverse)
import Effect (Effect)
import Effect.Exception (error)
import Effect.Exception (throwException)
import Math (pow) as Math
import React.Basic.DOM (aside)

-- Note to reader: Add your solutions to this file

third :: forall a. Array a -> Maybe a
third a = do
  first <- tail a
  second <- tail first
  head second

--foldM :: forall m a b. Monad m => (a -> b -> m a) -> a -> List b -> m a
--foldl :: forall   a b.            (a -> b ->   a) -> a -> List b ->   a

possibleSums' :: Array Int -> Array Int
possibleSums' bs = foldM f 0 bs
    where
        f :: Int -> Int -> Array Int
        f acc b = [acc + b]

possibleSums'' :: Array Int -> Array (Array Int)
possibleSums'' bs = foldM f [0] bs
    where
        f :: Array Int -> Int -> Array (Array Int)
        f acc b = [[(sum acc) + b], acc]

possibleSums''' :: Array Int -> Array Int
possibleSums''' bs = sort $ nub $ foldM f 0 bs
    where
        f :: Int -> Int -> Array Int
        f acc b = [acc + b, acc]

possibleSums :: Array Int -> Array Int
possibleSums bs = sort $ nub $ foldM (\acc b -> [acc + b, acc]) 0 bs

law1 e = do
  x <- e
  pure x

law1' e = do
    e

law2 :: Int -> Maybe Int
law2 y =do
  x <- pure y
  pure $ x * x

law2' :: Int -> Maybe Int
law2' y =do
  pure $ y * y


-- c1 :: Maybe Int -> Maybe Int -> Maybe Int
-- c1 m1 m2 = do
--   y <- do
--     x <- m1
--     m2
--   pure $ y + x

-- law3 :: Maybe Int -> Maybe Int -> Maybe Int
-- law3 a b = do
--   y <- do
--     x <- a
--     b
--   pure $ x - y

law3' :: Maybe Int -> Maybe Int -> Maybe Int
law3' a b = do
  y <- b
  x <- a  
  pure $ x - y

law3'' :: Maybe Int -> Maybe Int -> Maybe Int
law3'' a b = do
  x <- a
  do
    y <- b
    pure $ x - y


predicate :: forall a. a -> Maybe Boolean
predicate a = Just true

--foldM :: forall m a b. Monad m => (a -> b -> m a) -> a -> List b -> m a
--foldM :: forall m a b. Monad m => (Array a -> b -> Maybe Array a) -> Array a -> List b -> Maybe Array a
-- filterM :: forall m a. Maybe m => (a -> Maybe Boolean) -> List a -> Maybe (List a)
-- List (m a) -> (a -> m Boolean) -> List (m a)
-- List (Maybe a) -> (Maybe a -> Maybe Boolean) -> List (Maybe a)


--foldM :: forall m a b. Monad m => (a -> b -> m a) -> a -> List b -> m a
--foldM :: forall m acc b. Monad m => (acc -> b -> m acc) -> acc -> List b -> m acc
--foldM :: forall m acc b. Monad m => (List a -> b -> Maybe (List a)) -> List a -> List b -> Maybe (List a)

filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM f as = foldM ff Nil $ reverse as
    where
        ff :: List a -> a -> m (List a)
        ff acc b = do
            is <- f b
            pure if is
                then b:acc
                else acc


exceptionDivide :: Int -> Int -> Effect Int
exceptionDivide _ 0 = throwException $ error "div zero"
exceptionDivide a b = pure $ a / b

estimatePi :: Int -> Number
estimatePi n = run do
    ref <- new { x: 0.0, n: 1 }
    for 1 n \_ ->
      modify
        ( \o ->
            { x: o.x + (series $ toNumber o.n)
            , n: o.n + 1
            }
        )
        ref
    final <- read ref
    pure $ 4.0 * final.x
      where
        series :: Number -> Number
        series k = (Math.pow (-1.0) $ k + 1.0) / (2.0*k - 1.0)

fibonacci :: Int -> Int
fibonacci n = run do
    ref <- new { a: 0, b: 1 }
    for 0 n \_ ->
      modify
        ( \o ->
            { b: o.a + o.b
            , a: o.b
            }
        )
        ref
    final <- read ref
    pure final.a

fibonacci' :: Int -> Int
fibonacci' 0 = 0
fibonacci' 1 = 1
fibonacci' n =
  run
    ( do
        x <- new 0
        y <- new 1
        for 2 n \k -> do
          x' <- read x
          y' <- read y
          _ <- write (x' + y') y
          write y' x
        x' <- read x
        y' <- read y
        pure $ x' + y'
    )