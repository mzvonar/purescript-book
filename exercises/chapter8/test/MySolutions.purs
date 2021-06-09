module Test.MySolutions where

import Data.Maybe
import Prelude

import Data.Array (head, tail, sort, nub)
import Data.Foldable (foldM, sum)
import React.Basic.DOM (aside)
import Data.List (List(..), (:), filter, reverse)
-- import Data.List.Lazy.Types (List(..))
import Control.Plus (empty)
import Control.Apply (lift2)

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

-- filterM f as = do
--   a <- pure as
--   is <- f a
--   if is
--     then a
--     else empty  

-- filterM predicate as