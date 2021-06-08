module Test.MySolutions where

import Data.Maybe
import Prelude

import Control.Apply (lift2)
import Data.AddressBook (Address, address)
import Data.AddressBook.Validation (Errors, matches)
import Data.Generic.Rep (class Generic)
-- import Data.Newtype (traverse)
import Data.Show.Generic (genericShow)
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (class Foldable, class Traversable, foldl, foldr, foldMap, traverse, sequence)
import Data.Validation.Semigroup (V)

-- Note to reader: Add your solutions to this file

addMaybe :: forall a. Semiring a => Maybe a -> Maybe a -> Maybe a
addMaybe = lift2 (+)

subMaybe :: forall a. Ring a => Maybe a -> Maybe a -> Maybe a
subMaybe = lift2 (-)

mulMaybe :: forall a. Semiring a => Maybe a -> Maybe a -> Maybe a
mulMaybe = lift2 (*)

divMaybe :: forall a. EuclideanRing a => Maybe a -> Maybe a -> Maybe a
divMaybe = lift2 div


addApply :: forall f a. Apply f => Semiring a => f a -> f a -> f a
addApply = lift2 (+)

subApply :: forall f a. Apply f => Ring a => f a -> f a -> f a
subApply = lift2 (-)

mulApply :: forall f a. Apply f => Semiring a => f a -> f a -> f a
mulApply = lift2 (*)

divApply :: forall f a. Apply f => EuclideanRing a => f a -> f a -> f a
divApply = lift2 div


combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing
-- combineMaybe (Just a) = ado
--   x <- a
--   in pure x
combineMaybe (Just a) = Just <$> a


stateRegex :: Regex
stateRegex = unsafeRegex "^[a-zA-Z]{2}$" noFlags

nonEmptyRegex :: Regex
nonEmptyRegex = unsafeRegex "\\S+" noFlags

validateAddressImproved :: Address -> V Errors Address
validateAddressImproved a =
  address <$> matches "Street" nonEmptyRegex a.street
          <*> matches "City" nonEmptyRegex a.city
          <*> matches "State" stateRegex a.state


data Tree a = Leaf | Branch (Tree a) a (Tree a)

derive instance genericTree :: Show a => Generic (Tree a) _

instance showTree :: Show a => Show (Tree a) where
  show t = genericShow t

derive instance eqTree :: Eq a => Eq (Tree a)

instance functorTree :: Functor Tree where
  map _ Leaf = Leaf
  map f (Branch l v r) = Branch (map f l) (f v) (map f r)

instance foldableTree :: Foldable Tree where
  foldl _ b Leaf = b
  foldl f b (Branch l v r) = foldl f (f (foldl f b l) v) r

  foldr _ b Leaf = b
  foldr f b (Branch l v r) = foldr f (f v (foldr f b r)) l

  
  foldMap _ Leaf = mempty
  foldMap f (Branch l v r) = foldMap f l <> f v <> foldMap f r
    


instance traversableTree :: Traversable Tree where
  traverse _ Leaf = pure Leaf
  traverse f (Branch l v r) = Branch <$> traverse f l <*> f v <*> traverse f r

  sequence :: forall a m. Applicative m => Tree (m a) -> m (Tree a)
  sequence Leaf = pure Leaf
  sequence (Branch l v r) = Branch <$> sequence l <*> v <*> sequence r